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
;; A layer on top of ports for reading and writing bits in the
;; entropy-coded section of a JPEG.  This isn't a general bit-port
;; facility because it handles byte stuffing.
;;
;;; Code:

(define-module (jpeg bit-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:export (make-bit-port
            read-bits
            read-bit
            read-signed-bits
            put-bits
            flush-bits))

(define (make-bit-port port)
  ;; Bit count, values, and the port
  (vector 0 0 port))

(define (next-u8 port)
  (let ((u8 (get-u8 port)))
    (cond
     ((eof-object? u8)
      (error "Got EOF while reading bits"))
     ((eqv? u8 #xff)
      (let ((u8 (get-u8 port)))
        (unless (eqv? 0 u8)
          (if (eof-object? u8)
              (error "Got EOF while reading bits")
              (error "Found marker while reading bits"))))))
    u8))

(define-inlinable (read-bits bit-port n)
  (match bit-port
    (#(count bits port)
     (let lp ((count count) (bits bits))
       (cond
        ((<= n count)
         (vector-set! bit-port 0 (- count n))
         (logand (ash bits (- n count)) (1- (ash 1 n))))
        (else
         (let* ((u8 (next-u8 port))
                ;; We never need more than 16 bits in the buffer.
                (bits (+ (logand (ash bits 8) #xffff) u8)))
           (vector-set! bit-port 1 bits)
           (lp (+ count 8) bits))))))))

(define (read-bit bit-port)
  (read-bits bit-port 1))

(define (read-signed-bits bit-port n)
  (let ((bits (read-bits bit-port n)))
    (if (< bits (ash 1 (1- n)))
        (+ (ash -1 n) 1 bits)
        bits)))

(define (put-u8/stuff port u8)
  (put-u8 port u8)
  (when (eqv? u8 #xff)
    (put-u8 port 0)))

(define (put-bits bit-port bits len)
  (cond
   ((negative? bits)
    (put-bits bit-port (- bits (1+ (ash -1 len))) len))
   (else
    (match bit-port
      (#(count buf port)
       (let lp ((count count) (buf buf) (bits bits) (len len))
         (cond
          ((< (+ count len) 8)
           (vector-set! bit-port 0 (+ count len))
           (vector-set! bit-port 1 (logior (ash buf len) bits)))
          (else
           (let* ((head-len (- 8 count))
                  (head-bits (logand (ash bits (- head-len len))
                                     (1- (ash 1 head-len))))
                  (tail-len (- len head-len))
                  (tail-bits (logand bits (1- (ash 1 tail-len)))))
             (put-u8/stuff port (logior (ash buf head-len) head-bits))
             (lp 0 0 tail-bits tail-len))))))))))

(define (flush-bits bit-port)
  (match bit-port
    (#(count bits port)
     (unless (zero? count)
       ;; Pad remaining bits with 1, and stuff as needed.
       (let ((bits (logand #xff (logior (ash -1 count) bits))))
         (put-u8/stuff port bits))
       (vector-set! bit-port 0 0)
       (vector-set! bit-port 1 0)))))
