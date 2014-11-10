#!/usr/bin/env guile
!#

(use-modules (jpeg) (jpeg jfif) (jpeg array) (ice-9 match) (srfi srfi-11))

(define (dct-coefficients-for-constant-level-shifted-value value)
  ;; All AC coefficients zero == constant value.
  (let ((v (make-vector 64 0)))
    (vector-set! v 0 (* 1/4 1/2 value 64))
    v))

(define *replacement-chrominance*
  ;; Zero.
  (dct-coefficients-for-constant-level-shifted-value 0))

(define *replacement-luminance*
  ;; 128, a middle grey.
  (dct-coefficients-for-constant-level-shifted-value 0))

(define (replace-components frame mcu-array k)
  (let* ((comp (vector-ref (frame-components frame) k))
         (desamp-x (/ (frame-samp-x frame) (component-samp-x comp)))
         (desamp-y (/ (frame-samp-y frame) (component-samp-y comp)))
         (new-height (ceiling/ (frame-y frame) desamp-y))
         (new-width (ceiling/ (frame-x frame) desamp-x)))
    (make-jpeg
     (make-frame #f 8 new-height new-width
                 (array-unfold
                  (lambda (k)
                    (make-component k k 1 1 (if (zero? k) 0 1)))
                  (array-dimensions (frame-components frame)))
                 1 1)
     '()
     (array-unfold
      (lambda (i j)
        (array-unfold
         (lambda (k*)
           (array-unfold
            (lambda (zi zj)
              (cond
               ((= k k*)
                (let-values (((i i*) (euclidean/ i (component-samp-y comp)))
                             ((j j*) (euclidean/ j (component-samp-x comp))))
                  (array-ref (array-ref (array-ref mcu-array i j) k) i* j*)))
               ((= k* 0) *replacement-luminance*)
               (else *replacement-chrominance*)))
            (list 1 1)))
         (list (frame-component-count frame))))
      (list (ceiling/ new-height 8) (ceiling/ new-width 8))))))

(define (split-jpeg file)
  (let* ((stem (substring file 0
                          (or (string-index-right file #\.)
                              (string-length file))))
         (jpeg (read-jpeg file)))
    (let lp ((k 0))
      (when (< k (frame-component-count (jpeg-frame jpeg)))
        (write-jpeg (format #f "~a-~a.jpeg" stem k)
                    (replace-components (jpeg-frame jpeg)
                                        (jpeg-mcu-array jpeg)
                                        k))
        (lp (1+ k))))))

(when (batch-mode?)
  (match (program-arguments)
    ((_ file) (split-jpeg file))
    (_ (error "usage: split-jpeg.scm JPEG"))))
