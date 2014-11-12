#!/usr/bin/env guile
!#

(use-modules (jpeg jfif) (jpeg transform) (ice-9 match))

(define (transpose-jpeg file)
  (let* ((stem (substring file 0
                          (or (string-index-right file #\.)
                              (string-length file)))))
    (write-jpeg (format #f "~a-transposed.jpeg" stem)
                (transpose (read-jpeg file)))))

(when (batch-mode?)
  (match (program-arguments)
    ((_ file) (transpose-jpeg file))
    (_ (error "usage: transpose-jpeg.scm JPEG"))))
