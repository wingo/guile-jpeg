#!/usr/bin/env guile
!#

(use-modules (jpeg jfif) (jpeg array) (ice-9 match))

(define (crop frame misc mcu-array x y w h)
  (make-jpeg
   (make-frame #f 8 h w (frame-components frame)
               (frame-samp-x frame) (frame-samp-y frame))
   misc
   (array-unfold
    (lambda (i j) (array-ref mcu-array (+ i y) (+ j x)))
    (list (ceiling/ h (* (frame-samp-y frame) 8))
          (ceiling/ w (* (frame-samp-x frame) 8))))))

(define (crop-jpeg file x y w h)
  (let* ((stem (substring file 0
                          (or (string-index-right file #\.)
                              (string-length file))))
         (jpeg (read-jpeg file))
         (mcu-width (* (frame-samp-x (jpeg-frame jpeg)) 8))
         (mcu-height (* (frame-samp-y (jpeg-frame jpeg)) 8)))
    (unless (zero? (remainder x mcu-width))
      (error "x should be on mcu boundary" x mcu-width))
    (unless (zero? (remainder y mcu-height))
      (error "y should be on mcu boundary" y mcu-height))
    (write-jpeg (format #f "~a-cropped.jpeg" stem)
                (crop (jpeg-frame jpeg)
                      (jpeg-misc-segments jpeg)
                      (jpeg-mcu-array jpeg)
                      (/ x mcu-width) (/ y mcu-height)
                      w h))))

(define (->int x)
  (let ((i (string->number x)))
    (unless (and (number? i) (exact-integer? i) (>= i 0))
      (error "expected an integer" i))
    i))

(when (batch-mode?)
  (match (program-arguments)
    ((_ file x y w h) (crop-jpeg file (->int x) (->int y) (->int w) (->int h)))
    (_ (error "usage: crop-jpeg.scm JPEG X Y WIDTH HEIGHT"))))
