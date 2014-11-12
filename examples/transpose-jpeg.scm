#!/usr/bin/env guile
!#

(use-modules (jpeg jfif) (jpeg array) (ice-9 match) (srfi srfi-11))

(define (transpose-block block)
  (array-unfold
   (lambda (n)
     (let-values (((i j) (euclidean/ n 8)))
       ;; Swapping i and j does the transpose.
       (array-ref block (+ (* j 8) i))))
   (array-dimensions block)))

(define (array-transpose array)
  (array-unfold
   (lambda (i j) (array-ref array j i))
   (reverse (array-dimensions array))))

(define (transpose frame mcu-array)
  (make-jpeg
   (make-frame #f 8 (frame-x frame) (frame-y frame)
               (array-map-values
                (lambda (component)
                  (make-component (component-id component)
                                  (component-index component)
                                  (component-samp-y component)
                                  (component-samp-x component)
                                  (component-q-table component)))
                (frame-components frame))
               (frame-samp-y frame) (frame-samp-x frame))
   '()
   (array-map-values
    (lambda (components)
      (array-map-values
       (lambda (blocks)
         (array-map-values transpose-block (array-transpose blocks)))
       components))
    (array-transpose mcu-array))))

(define (transpose-jpeg file)
  (let* ((stem (substring file 0
                          (or (string-index-right file #\.)
                              (string-length file))))
         (jpeg (read-jpeg file)))
    (write-jpeg (format #f "~a-transponsed.jpeg" stem)
                (transpose (jpeg-frame jpeg)
                           (jpeg-mcu-array jpeg)))))

(when (batch-mode?)
  (match (program-arguments)
    ((_ file) (transpose-jpeg file))
    (_ (error "usage: transpose-jpeg.scm JPEG"))))
