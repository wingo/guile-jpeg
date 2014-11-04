;; guile-jpeg
;; Copyright (C) 2014 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Forward and inverse JPEG discrete cosine transforms.
;;
;;; Code:

(define-module (jpeg dct)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (jpeg array)
  #:use-module (jpeg jfif)
  #:use-module (jpeg pixbufs)
  #:export (jpeg->planar-image
            planar-image->jpeg))


(define-syntax eval-at-compile-time
  (lambda (x)
    (syntax-case x ()
      ((eval-at-compile-time expr)
       (datum->syntax #'eval-at-compile-time
                      (primitive-eval (syntax->datum #'expr)))))))

(define fdct-coefficients
  (eval-at-compile-time
   (let ((pi (* 2 (acos 0))))
     (array-unfold
      (lambda (u v)
        (typed-array-unfold
         (lambda (k)
           (call-with-values (lambda () (euclidean/ k 8))
             (lambda (i j)
               (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                     (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                 (* 1/4 Cu Cv
                    (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                    (cos (/ (* (+ (* 2 j) 1) v pi) 16)))))))
         'f32 (list (* 8 8))))
      (list 8 8)))))

(define idct-coefficients
  (eval-at-compile-time
   (let ((pi (* 2 (acos 0))))
     (array-unfold
      (lambda (i j)
        (typed-array-unfold
         (lambda (k)
           (call-with-values (lambda () (euclidean/ k 8))
             (lambda (u v)
               (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                     (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                 (* 1/4 Cu Cv
                    (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                    (cos (/ (* (+ (* 2 j) 1) v pi) 16)))))))
         'f32 (list (* 8 8))))
      (list 8 8)))))

(define (idct-block block plane pos stride)
  (define (idct i j)
    (array-fold
     (lambda (k coeff sum)
       (+ sum (* coeff (vector-ref block k))))
     (array-ref idct-coefficients i j)
     0.0))
  (let lp ((i 0) (pos pos))
    (when (< i 8)
      (let lp ((j 0))
        (when (< j 8)
          (let* ((s (idct i j))
                 (sq (cond
                      ((< s -128.0) 0)
                      ((> s 127.0) 255)
                      (else (+ 128 (inexact->exact (round s)))))))
            (bytevector-u8-set! plane (+ pos j) sq))
          (lp (1+ j))))
      (lp (1+ i) (+ pos stride)))))

(define* (vector-fold* f seed v #:key (key identity))
  (vector-fold (lambda (i seed elt) (f (key elt) seed)) seed v))

(define (jpeg->planar-image jpeg)
  (match jpeg
    (($ <jpeg> frame misc-segments mcu-array)
     (let ((mcu-width (frame-mcu-width frame))
           (mcu-height (frame-mcu-height frame)))
       (make-planar-image
        (frame-x frame)
        (frame-y frame)
        (* mcu-width (frame-samp-x frame) 8)
        (* mcu-height (frame-samp-y frame) 8)
        (vector-map
         (lambda (k component)
           (let* ((samp-x (component-samp-x component))
                  (samp-y (component-samp-y component))
                  (block-width (* mcu-width samp-x))
                  (block-height (* mcu-height samp-y))
                  (sample-width (* block-width 8))
                  (sample-height (* block-height 8))
                  (plane (make-bytevector (* sample-width sample-height) 0)))
             (array-for-each
              (lambda (i j mcu)
                (let* ((mcu-y (* i samp-y 8))
                       (mcu-x  (* j samp-x 8))
                       (offset (+ (* mcu-y sample-width) mcu-x)))
                  (array-for-each
                   (lambda (i j block)
                     (let ((offset (+ offset (* i 8 sample-width) (* j 8))))
                       (idct-block block plane offset sample-width)))
                   (vector-ref mcu k))))
              mcu-array)
             (make-plane sample-width sample-height plane)))
         (frame-components frame)))))))

;; Tables K.1 and K.2 from the JPEG specification.
(define *standard-luminance-q-table*
  #(16 11 10 16 24 40 51 61
    12 12 14 19 26 58 60 55
    14 13 16 24 40 57 69 56
    14 17 22 29 51 87 80 62
    18 22 37 56 68 109 103 77
    24 35 55 64 81 104 113 92
    49 64 78 87 103 121 120 101
    72 92 95 98 112 100 103 99))

(define *standard-chrominance-q-table*
  #(17 18 24 47 99 99 99 99
    18 21 26 66 99 99 99 99
    24 26 56 99 99 99 99 99
    47 66 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99))

;; As libjpeg does, we consider the above tables to be quality 50, on a
;; scale from 1 (terrible) to 100 (great).  We linearly scale the values
;; so that at quality 100, all values are 1, and at quality 1 all values
;; are 255.
(define (q-tables-for-quality quality)
  ;; This mapping of quality to a linear scale is also from libjpeg.
  (let* ((quality (exact->inexact quality)) ;; allow divide by zero -> inf
         (linear-scale (if (< quality 50)
                           (/ 50. quality)
                           (- 1 (/ (- quality 50) 50)))))
    (define (scale x)
      (let ((x (* x linear-scale)))
        (cond
         ((< x 1) 1)
         ((> x 255) 255)
         (else (inexact->exact (round x))))))
    (vector (array-map-values scale *standard-luminance-q-table*)
            (array-map-values scale *standard-chrominance-q-table*)
            #f
            #f)))

(define (fdct-block plane pos stride q-table)
  (define (fdct v u)
    (let ((coeffs (array-ref fdct-coefficients v u)))
      (let lp ((i 0) (pos pos) (sum 0.0))
        (if (< i 8)
            (lp (1+ i)
                (+ pos stride)
                (let lp ((j 0) (k (* i 8)) (pos pos) (sum sum))
                  (if (< j 8)
                      (let ((coeff (f32vector-ref coeffs k))
                            (sample (- (bytevector-u8-ref plane pos) 128)))
                        (lp (1+ j)
                            (1+ k)
                            (1+ pos)
                            (+ sum (* coeff sample))))
                      sum)))
            sum))))
  (vector-unfold
   (lambda (k)
     (let ((v (ash k -3))
           (u (logand k 7))
           (q (vector-ref q-table k)))
       (let ((Svu (fdct v u)))
         (* q (inexact->exact (round (/ Svu q)))))))
   (* 8 8)))

(define* (planar-image->jpeg yuv #:key (quality 85)
                             (q-tables (q-tables-for-quality quality))
                             ;; In JFIF baseline JPEG images, component
                             ;; 0 is Y', and components 1 and 2 are Cb
                             ;; and Cr.  Assign the first quantization
                             ;; table for luminance, and the second for
                             ;; chrominance.
                             (plane-q-table (lambda (i) (if (zero? i) 0 1))))
  (match yuv
    (($ <planar-image> width height canvas-width canvas-height planes)
     (define (x-subsampling plane) (/ canvas-width (plane-width plane)))
     (define (y-subsampling plane) (/ canvas-height (plane-height plane)))
     (let ((samp-x (vector-fold* lcm 1 planes #:key x-subsampling))
           (samp-y (vector-fold* lcm 1 planes #:key y-subsampling)))
       (define (plane-samp-x plane)
         (* samp-x (/ (plane-width plane) canvas-width)))
       (define (plane-samp-y plane)
         (* samp-y (/ (plane-height plane) canvas-height)))
       (let ((components
              (array-unfold
               (lambda (i)
                 (let* ((plane (vector-ref planes i))
                        (samp-x (plane-samp-x plane))
                        (samp-y (plane-samp-y plane)))
                   (make-component i i samp-x samp-y (plane-q-table i))))
               (array-dimensions planes))))
         (make-jpeg
          (make-frame #f 8 height width components samp-x samp-y)
          '()
          (array-unfold
           (lambda (i j)
             (array-map-values
              (lambda (component)
                (match (vector-ref planes (component-index component))
                  (($ <plane> plane-width plane-height samples)
                   (let ((samp-y (component-samp-y component))
                         (samp-x (component-samp-x component)))
                     (array-unfold
                      (lambda (y x)
                        (let* ((pos (+ (* (+ (* i samp-y) y) 8 plane-width)
                                       (* (+ (* j samp-x) x) 8)))
                               (q-table-index (component-q-table component))
                               (q-table (vector-ref q-tables q-table-index)))
                          (fdct-block samples pos plane-width q-table)))
                      (list samp-y samp-x))))))
              components))
           (list (/ canvas-height 8 samp-y) (/ canvas-width 8 samp-x)))))))))
