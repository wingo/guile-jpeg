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
;; Array utilities.
;;
;;; Code:

(define-module (jpeg array)
  #:use-module (ice-9 match)
  #:export (typed-array-unfold
            array-unfold
            array-fold
            array-fold-values
            array-for-each-value
            array-map-values)
  #:replace (array-for-each))

(define typed-array-unfold
  (case-lambda
    ((f type dims)
     (match dims
       ((height width)
        (let ((ret (make-typed-array type *unspecified* height width)))
          (let lp ((i 0))
            (when (< i height)
              (let lp ((j 0))
                (when (< j width)
                  (array-set! ret (f i j) i j)
                  (lp (1+ j))))
              (lp (1+ i))))
          ret))
       (_
        (let ((ret (apply make-typed-array type *unspecified* dims)))
          (let init ((head '()) (dims dims))
            (match dims
              (() (apply array-set! ret (apply f head) head))
              ((dim . dims)
               (let lp ((i 0))
                 (when (< i dim)
                   (init (append head (list i)) dims)
                   (lp (1+ i)))))))
          ret))))
    ((f type dims . seeds)
     (let ((ret (apply make-typed-array type *unspecified* dims)))
       (apply values
              ret
              (let init ((head '()) (dims dims) (seeds seeds))
                (match dims
                  (()
                   (call-with-values
                       (lambda () (apply f (append head seeds)))
                     (lambda (val . seeds)
                       (apply array-set! ret val head)
                       seeds)))
                  ((dim . dims)
                   (let lp ((i 0) (seeds seeds))
                     (if (< i dim)
                         (lp (1+ i)
                             (init (append head (list i)) dims seeds))
                         seeds))))))))))

(define (array-unfold f dims . seeds)
  (apply typed-array-unfold f #t dims seeds))

(define array-fold
  (case-lambda
    ((f array seed)
     (match (array-dimensions array)
       ((length)
        (let lp ((i 0) (seed seed))
          (if (< i length)
              (lp (1+ i)
                  (f i (array-ref array i) seed))
              seed)))
       ((height width)
        (let lp ((i 0) (seed seed))
          (if (< i height)
              (lp (1+ i)
                  (let lp ((j 0) (seed seed))
                    (if (< j width)
                        (lp (1+ j) (f i j (array-ref array i j) seed))
                        seed)))
              seed)))
       (dims
        (let fold ((head '()) (dims dims) (seed seed))
          (match dims
            (()
             (apply f (append head (list (apply array-ref array head) seed))))
            ((dim . dims)
             (let lp ((i 0) (seed seed))
               (if (< i dim)
                   (lp (1+ i)
                       (fold (append head (list i)) dims seed))
                   seed))))))))
    ((f array . seeds)
     (let fold ((head '()) (dims (array-dimensions array)) (seeds seeds))
       (match dims
         (()
          (let ((elt (apply array-ref array head)))
            (apply f (append head (cons elt seeds)))))
         ((dim . dims)
          (let lp ((i 0) (seeds seeds))
            (if (< i dim)
                (call-with-values
                    (lambda ()
                      (fold (append head (list i)) dims seeds))
                  (lambda seeds
                    (lp (1+ i) seeds)))
                (apply values seeds)))))))))

(define array-fold-values
  (case-lambda
    ((f array seed)
     (match (array-rank array)
       (0 (f (array-ref array) seed))
       (1 (array-fold (lambda (i elt seed) (f elt seed)) array seed))
       (2 (array-fold (lambda (i j elt seed) (f elt seed)) array seed))
       (rank (array-fold (lambda args (apply f (list-tail args rank)))
                         array seed))))
    ((f array . seeds)
     (let ((rank (array-rank array)))
       (apply array-fold
              (lambda args
                (apply f (list-tail args rank)))
              array seeds)))))

(define (array-for-each f array)
  (match (array-rank array)
    (0 (f (array-ref array)) *unspecified*)
    (1 (array-fold (lambda (i elt seed) (f i elt) *unspecified*)
                   array *unspecified*))
    (2 (array-fold (lambda (i j elt seed) (f i j elt) *unspecified*)
                   array *unspecified*))
    (rank (array-fold (lambda args
                        (apply f (append (list-head args (1+ rank))))
                        *unspecified*)
                      array *unspecified*))))

(define (array-for-each-value f array)
  (array-fold-values (lambda (x v) (f x) *unspecified*)
                     array *unspecified*))

(define (array-map-values f array . arrays)
  (array-unfold
   (match arrays
     (()
      (match (array-rank array)
        (1 (lambda (i) (f (array-ref array i))))
        (2 (lambda (i j) (f (array-ref array i j))))
        (_ (lambda dims (f (apply array-ref array dims))))))
     ((array2)
      (match (array-rank array)
        (1 (lambda (i) (f (array-ref array i) (array-ref array2 i))))
        (2 (lambda (i j) (f (array-ref array i j) (array-ref array2 i j))))
        (_ (lambda dims (f (apply array-ref array dims)
                           (apply array-ref array dims))))))
     (arrays
      (lambda dims
        (apply f (map (lambda (array) (apply array-ref array dims))
                      (cons array arrays))))))
   (array-dimensions array)))
