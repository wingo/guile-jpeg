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
;; JPEG Huffman coding.
;;
;;; Code:

(define-module (jpeg huffman)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (jpeg array)
  #:use-module (jpeg bit-ports)
  #:export (make-huffman-table
            print-huffman-table
            read-huffman-coded-value
            compute-huffman-table-for-freqs))

(define (make-huffman-table size-counts values)
  (let* ((count (bytevector-length values))
         (size-offsets (make-vector 16 #f))
         (sizes (make-bytevector count 0))
         (codes (make-vector count #f))
         (value-indexes (make-vector 256 #f))
         (max-codes (make-vector 16 -1)))
    ;; A reverse map from value to index.
    (array-for-each
     (lambda (i value)
       (vector-set! value-indexes value i))
     values)
    ;; Compute sizes for each value.
    (let lp ((size 0) (offset 0))
      (when (< size 16)
        (vector-set! size-offsets size offset)
        (let ((size-count (bytevector-u8-ref size-counts size)))
          (let lp ((i 0))
            (when (< i size-count)
              (bytevector-u8-set! sizes (+ offset i) (1+ size))
              (lp (1+ i))))
          (lp (1+ size) (+ offset size-count)))))
    ;; Compute codes.  This is the algorithm from Annex C, verbatim.
    (let lp ((k 0) (code 0) (si (bytevector-u8-ref sizes 0)))
      (vector-set! max-codes (1- si) code)
      (vector-set! codes k code)
      (let ((code (1+ code)) (k (1+ k)))
        (when (< k (bytevector-length sizes))
          (let lp2 ((code code) (si si))
            (if (= (bytevector-u8-ref sizes k) si)
                (lp k code si)
                (lp2 (ash code 1) (1+ si)))))))
    ;; Done.
    (vector size-counts size-offsets
            values value-indexes sizes codes max-codes)))

(define (print-huffman-table table)
  (match table
    (#(size-counts size-offsets
       values value-indexes sizes codes max-codes)
     (let lp ((n 0))
       (when (< n (bytevector-length values))
         (let ((si (bytevector-u8-ref sizes n))
               (code (vector-ref codes n))
               (value (bytevector-u8-ref values n)))
           (format #t "~a: #*~v,'0b -> #x~2,0x\n" n si code value)
           (lp (1+ n))))))))

(define (read-huffman-coded-value bit-port table)
  ;(print-huffman-table table)
  (match table
    (#(size-counts size-offsets
       values value-indexes sizes codes max-codes)
     (let lp ((size-idx 0) (code (read-bit bit-port)))
       (cond
        ((<= code (vector-ref max-codes size-idx))
         (let* ((size-offset (vector-ref size-offsets size-idx))
                (idx (+ size-offset (- code (vector-ref codes size-offset)))))
           (unless (>= code (vector-ref codes size-offset))
             (error "impossaurus"))
           (bytevector-u8-ref values idx)))
        (else
         (lp (1+ size-idx) (+ (ash code 1) (read-bit bit-port)))))))))

(define (vector-inc! v idx addend)
  (vector-set! v idx (+ (vector-ref v idx) addend)))

(define (compute-huffman-code-sizes-for-freqs freqs)
  (let ((sizes (make-bytevector 257 0))
        (others (make-vector 257 #f))
        (max-size 0))
    (define (inc-size! code)
      (let ((size (1+ (bytevector-u8-ref sizes code))))
        (bytevector-u8-set! sizes code size)
        (when (< max-size size)
          (set! max-size size))))
    (define (find-least-idx)
      (let lp ((i 0) (least-idx #f))
        (if (< i 257)
            (lp (1+ i)
                (let ((x (vector-ref freqs i)))
                  (cond ((zero? x) least-idx)
                        ((not least-idx) i)
                        ((<= x (vector-ref freqs least-idx)) i)
                        (else least-idx))))
            least-idx)))
    (define (find-next-least least-idx)
      (let lp ((i 0) (next-least-idx #f))
        (if (< i 257)
            (lp (1+ i)
                (let ((x (vector-ref freqs i)))
                  (cond ((zero? x) next-least-idx)
                        ((= i least-idx) next-least-idx)
                        ((not next-least-idx) i)
                        ((<= x (vector-ref freqs next-least-idx)) i)
                        (else next-least-idx))))
            next-least-idx)))
    (let lp ((v1 256))
      (cond
       ((find-next-least v1)
        => (lambda (v2)
             (vector-inc! freqs v1 (vector-ref freqs v2))
             (vector-set! freqs v2 0)
             (let lp ((v1 v1))
               (inc-size! v1)
               (cond
                ((vector-ref others v1) => lp)
                (else
                 (vector-set! others v1 v2)
                 (let lp ((v2 v2))
                   (inc-size! v2)
                   (cond
                    ((vector-ref others v2) => lp))))))
             (lp (find-least-idx))))
       (else (values sizes max-size))))))

(define (compute-huffman-table-for-freqs freqs)
  (define (bytevector-truncate bv len)
    (if (< len (bytevector-length bv))
        (let ((bv* (make-bytevector len)))
          (bytevector-copy! bv 0 bv* 0 len)
          bv*)
        bv))
  (call-with-values (lambda ()
                      (let ((copy (make-vector 257)))
                        (vector-move-left! freqs 0 256 copy 0)
                        ;; Add dummy entry.
                        (vector-set! copy 256 1)
                        (compute-huffman-code-sizes-for-freqs copy)))
    (lambda (sizes max-size)
      (let ((size-counts (make-bytevector (max max-size 16) 0)))
        (define (inc-size-count! size n)
          (bytevector-u8-set! size-counts size
                              (+ (bytevector-u8-ref size-counts size) n)))
        (let count-bits ((i 0))
          (when (< i 257)
            (let ((size (bytevector-u8-ref sizes i)))
              (unless (zero? size)
                (inc-size-count! (1- size) 1)))
            (count-bits (1+ i))))
        (let adjust-bits ((i (1- max-size)))
          (cond
           ((zero? (bytevector-u8-ref size-counts i))
            (adjust-bits (1- i)))
           ((< i 16)
            ;; We're done.  Remove the dummy entry.
            (inc-size-count! i -1))
           (else
            ;; We have a code that is > 16 bits long.  Reshuffle the
            ;; tree to fit the code into 16 bits.
            (let lp ((j (1- i)))
              (cond
               ((zero? (bytevector-u8-ref size-counts j))
                (lp (1- j)))
               (else
                (inc-size-count! i -2)
                (inc-size-count! (1- i) 1)
                (inc-size-count! (1+ j) 2)
                (inc-size-count! j -1))))
            (adjust-bits i))))
        ;; Sort values, then compute codes.
        (let* ((count (array-fold-values + size-counts 0))
               (values (make-bytevector count 0)))
          (let visit-size ((size 1) (k 0))
            (when (<= size max-size)
              (let visit-values ((j 0) (k k))
                (cond
                 ((= j 256)
                  (visit-size (1+ size) k))
                 ((= (bytevector-u8-ref sizes j) size)
                  (bytevector-u8-set! values k j)
                  (visit-values (1+ j) (1+ k)))
                 (else
                  (visit-values (1+ j) k))))))
          (make-huffman-table (bytevector-truncate size-counts 16)
                              values))))))
