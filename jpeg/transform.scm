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
;; Lossless JPEG transformations.
;;
;;; Code:

(define-module (jpeg transform)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (jpeg array)
  #:use-module (jpeg jfif)
  #:export (flip-horizontally
            flip-vertically
            transpose
            crop))

(define (crop jpeg x y width height)
  (let* ((mcu-width (* (frame-samp-x (jpeg-frame jpeg)) 8))
         (mcu-height (* (frame-samp-y (jpeg-frame jpeg)) 8)))
    (define (crop frame misc mcu-array i0 j0)
      (make-jpeg
       (make-frame #f 8 height width (frame-components frame)
                   (frame-samp-x frame) (frame-samp-y frame))
       misc
       (array-unfold
        (lambda (i j)
          (array-ref mcu-array (+ i0 i) (+ j0 j)))
        (list (ceiling/ height mcu-height)
              (ceiling/ width mcu-width)))))
    (unless (zero? (remainder x mcu-width))
      (error "x should be on mcu boundary" x mcu-width))
    (unless (zero? (remainder y mcu-height))
      (error "y should be on mcu boundary" y mcu-height))
    (crop (jpeg-frame jpeg)
          (jpeg-misc-segments jpeg)
          (jpeg-mcu-array jpeg)
          (/ y mcu-height) (/ x mcu-width))))

(define (transpose jpeg)
  (define (array-transpose array)
    (array-unfold
     (lambda (i j) (array-ref array j i))
     (reverse (array-dimensions array))))
  (define (transpose-block block)
    (array-unfold
     (lambda (n)
       (let-values (((i j) (euclidean/ n 8)))
         ;; Swapping i and j does the transpose.
         (array-ref block (+ (* j 8) i))))
     (array-dimensions block)))
  (let ((frame (jpeg-frame jpeg)))
    (make-jpeg
     (make-frame #f 8 (frame-x frame) (frame-y frame)
                 (array-map-values
                  (lambda (component)
                    ;; Swap samp-x and samp-y.
                    (make-component (component-id component)
                                    (component-index component)
                                    (component-samp-y component)
                                    (component-samp-x component)
                                    (component-q-table component)))
                  (frame-components frame))
                 (frame-samp-y frame) (frame-samp-x frame))
     (jpeg-misc-segments jpeg)
     (array-map-values
      (lambda (components)
        (array-map-values
         (lambda (blocks)
           (array-map-values transpose-block (array-transpose blocks)))
         components))
      (array-transpose (jpeg-mcu-array jpeg))))))

(define (flip-vertically jpeg)
  (define (array-flip array)
    (match (array-dimensions array)
      ((height width)
       (array-unfold
        (lambda (i j) (array-ref array (- height i 1) j))
        (list height width)))))
  (define (flip-block block)
    (array-unfold
     (lambda (n)
       (let-values (((i j) (euclidean/ n 8)))
         (let ((Svu (array-ref block (+ (* i 8) j))))
           (if (even? i) Svu (- Svu)))))
     (array-dimensions block)))
  (let ((frame (jpeg-frame jpeg)))
    (make-jpeg
     frame
     (jpeg-misc-segments jpeg)
     (array-map-values
      (lambda (components)
        (array-map-values
         (lambda (blocks)
           (array-map-values flip-block (array-flip blocks)))
         components))
      (array-flip mcu-array)))))

(define (flip-horizontally jpeg)
  (define (array-flip array)
    (match (array-dimensions array)
      ((height width)
       (array-unfold
        (lambda (i j) (array-ref array i (- width j 1)))
        (list height width)))))
  (define (flip-block block)
    (array-unfold
     (lambda (n)
       (let-values (((i j) (euclidean/ n 8)))
         (let ((Svu (array-ref block (+ (* i 8) j))))
           (if (even? j) Svu (- Svu)))))
     (array-dimensions block)))
  (let ((frame (jpeg-frame jpeg)))
    (make-jpeg
     frame
     (jpeg-misc-segments jpeg)
     (array-map-values
      (lambda (components)
        (array-map-values
         (lambda (blocks)
           (array-map-values flip-block (array-flip blocks)))
         components))
      (array-flip mcu-array)))))
