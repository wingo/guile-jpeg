#!/usr/bin/env guile
!#

(use-modules (jpeg jfif) (jpeg array) (ice-9 match) (jpeg transform))

(define (crop-jpeg file x y w h)
  (let* ((stem (substring file 0
                          (or (string-index-right file #\.)
                              (string-length file)))))
    (write-jpeg (format #f "~a-cropped.jpeg" stem)
                (crop (read-jpeg file) x y w h))))

(define (->int x)
  (let ((i (string->number x)))
    (unless (and (number? i) (exact-integer? i) (>= i 0))
      (error "expected an integer" i))
    i))

(when (batch-mode?)
  (match (program-arguments)
    ((_ file x y w h) (crop-jpeg file (->int x) (->int y) (->int w) (->int h)))
    (_ (error "usage: crop-jpeg.scm JPEG X Y WIDTH HEIGHT"))))
