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
;; Test for (jpeg).
;;
;;; Code:

(define-module (tests parse)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (jpeg)
  #:use-module (jpeg jfif))

(define (call-with-output-bytevector proc)
  (call-with-values open-bytevector-output-port
    (lambda (port get-bytes)
      (proc port)
      (get-bytes))))

(define jpeg
  (read-jpeg (string-append (getenv "GUILE_JPEG_TOP_SRCDIR")
                            "/tests/imag0029.normal.jpg")))

(define bytes
  (call-with-output-bytevector
   (lambda (port)
     (write-jpeg port jpeg))))

(define jpeg2
  (read-jpeg bytes))

(define (assert-same x y)
  (cond
   ((eqv? x y) #t)
   ((struct? x)
    (unless (struct? y)
      (error "types differ" x y))
    (unless (eq? (struct-vtable x) (struct-vtable y))
      (error "vtables differ" x y))
    (let* ((layout (struct-ref (struct-vtable x) vtable-index-layout))
           (nfields (/ (string-length (symbol->string layout)) 2)))
      (let ((i 0))
        (when (< i nfields)
          (assert-same (struct-ref x i) (struct-ref y i))))))
   ((array? x)
    (unless (array? y) (error "types differ" x y))
    (assert-same (array-shape x) (array-shape y))
    (assert-same (array-type x) (array-type y))
    (array-for-each assert-same x y))
   ((pair? x)
    (unless (pair? y) (error "types differ" x y))
    (assert-same (car x) (car y))
    (assert-same (cdr x) (cdr y)))
   (else
    (error "not same" x y))))

(assert-same jpeg jpeg2)
