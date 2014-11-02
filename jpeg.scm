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
;; A parser for JPEG.
;;
;;; Code:

(define-module (jpeg)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold filter-map))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (jpeg exif)
  #:use-module (jpeg array)
  #:export (jpeg-dimensions
            jpeg-dimensions-and-exif))




;; See http://www.w3.org/Graphics/JPEG/itu-t81.pdf for ITU
;; recommendation T.81, which is a freely-available version of the JPEG
;; specification.

;; JPEG := SOI FRAME EOI
;; FRAME := MISC* FHEADER SCAN DNL? SCAN ... 
;; SCAN := MISC* SHEADER ECS (RST ECS)*
;; FHEADER := SOF LEN PRECISION Y X COMP0 COMP1 ...
;; MISC := (DQT | DHT | DAC | DRI | COM | APP) LEN payload...
;; SHEADER := SOS LEN NCOMPONENTS SCOMP0 SCOMP1 ... SS SE A

(define (read-marker port)
  (let ((u8 (get-u8 port)))
    (unless (eqv? u8 #xff)
      (error "Unexpected byte while reading marker" u8)))
  (let lp ()
    (let ((u8 (get-u8 port)))
      (when (eof-object? u8)
        (error "End of file while reading marker"))
      (case u8
        ((#xff) (lp))
        ((0) (error "Expected a marker, got #xFF00"))
        (else (logior #xff00 u8))))))

(define (assert-marker port expected-marker)
  (let ((marker (read-marker port)))
    (unless (eqv? expected-marker marker)
      (error "Unexpected marker" marker expected-marker))))

(define (read-u8 port)
  (let* ((u8 (get-u8 port)))
    (when (eof-object? u8)
      (error "EOF while reading byte from port"))
    u8))

(define (read-u16 port)
  (let* ((msb (get-u8 port))
         (lsb (get-u8 port)))
    (when (eof-object? lsb)
      (error "EOF while reading two-byte value"))
    (logior (ash msb 8) lsb)))

(define (read-bytes port n)
  (let ((bytes (get-bytevector-n port n)))
    (unless (= (bytevector-length bytes) n)
      (error "EOF while reading bytes" n))
    bytes))

(define (read-soi port)
  (assert-marker port #xffd8))

(define-syntax eval-at-compile-time
  (lambda (x)
    (syntax-case x ()
      ((eval-at-compile-time expr)
       (datum->syntax #'eval-at-compile-time
                      (primitive-eval (syntax->datum #'expr)))))))

(define normal-order
  (eval-at-compile-time
   (let* ((width 8)
          (height 8)
          ;; The padding is to allow the 4-bit offsets in the AC
          ;; coefficient decode loop to increment "k" beyond 63.
          ;; Strictly speaking, at that point we should signal an error,
          ;; but perhaps it's better to keep on trucking.  This trick
          ;; was taken from libjpeg.
          (padding 16)
          (len (* width height))
          (res (make-bytevector (+ len padding) (1- len))))
     (let lp ((x 0) (y 0) (x-inc 1) (y-inc -1) (pos 0))
       (when (< pos len)
         (cond
          ((< x 0) (lp 0 y (- x-inc) (- y-inc) pos))
          ((< y 0) (lp x 0 (- x-inc) (- y-inc) pos))
          ((and (< x width) (< y height))
           (bytevector-u8-set! res pos (+ (* y height) x))
           (lp (+ x x-inc) (+ y y-inc) x-inc y-inc (1+ pos)))
          (else
           (lp (+ x x-inc) (+ y y-inc) x-inc y-inc pos)))))
     res)))

(define (read-q-table port len q-tables)
  (unless (>= len 3)
    (error "Invalid DQT segment length" len))
  (let* ((PT (read-u8 port))
         (Pq (ash PT -4))
         (Tq (logand PT #xf))
         (table (make-vector 64 #f)))
    (define (zigzag->normal idx) (bytevector-u8-ref normal-order idx))
    (unless (< Tq 4)
      (error "Bad Tq value" Tq))
    (case Pq
      ((0)
       (unless (= len (+ 3 64))
         (error "Invalid DQT segment length" len))
       (let lp ((n 0))
         (when (< n 64)
           (vector-set! table (zigzag->normal n) (read-u8 port))
           (lp (1+ n)))))
      ((1)
       (unless (= len (+ 3 128))
         (error "Invalid DQT segment length" len))
       (let lp ((n 0))
         (when (< n 64)
           (vector-set! table (zigzag->normal n) (read-u16 port))
           (lp (1+ n)))))
      (else
       (error "Bad Pq value" Pq)))
    (vector-set! q-tables Tq table)))

(define (compute-huffman-codes size-counts values)
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

(define (read-huffman-table port len dc-tables ac-tables)
  (unless (>= len 19)
    (error "Invalid DHT segment length" len))
  (let* ((T (read-u8 port))
         (Tc (ash T -4))
         (Th (logand T #xf))
         (size-counts (read-bytes port 16))
         (count (fold + 0 (bytevector->u8-list size-counts))))
    (unless (< Th 4)
      (error "Bad Th value" Th))
    (unless (= len (+ 19 count))
      (error "Invalid DHT segment length" len))
    (let* ((values (read-bytes port count))
           (table (compute-huffman-codes size-counts values)))
      (match Tc
        (0 (vector-set! dc-tables Th table))
        (1 (vector-set! ac-tables Th table))
        (_ (error "Bad Tc value" Tc))))))

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

(define-record-type <misc>
  (make-misc marker bytes)
  misc?
  (marker misc-marker)
  (bytes misc-bytes))

(define-record-type <params>
  (make-params q-tables dc-tables ac-tables restart-interval misc-segments)
  params?
  (q-tables q-tables)
  (dc-tables dc-tables)
  (ac-tables ac-tables)
  (restart-interval restart-interval)
  (misc-segments misc-segments))

(define *default-params*
  (make-params (make-vector 4 #f)
               (make-vector 4 #f)
               (make-vector 4 #f)
               0
               '()))

(define* (read-params port #:optional (outer-params *default-params*))
  (let* ((q-tables (vector-copy (q-tables outer-params)))
         (dc-tables (vector-copy (dc-tables outer-params)))
         (ac-tables (vector-copy (ac-tables outer-params)))
         (restart-interval (restart-interval outer-params))
         (misc-segments '())) ;; No sense inheriting this.
    (let lp ()
      (let ((marker (read-marker port)))
        (case marker
          ((#xffdb)                     ; DQT
           (let* ((len (read-u16 port)))
             (read-q-table port len q-tables)
             (lp)))
          ((#xffc4)                     ; DHT
           (let* ((len (read-u16 port)))
             (read-huffman-table port len dc-tables ac-tables)
             (lp)))
          ((#xffcc)                     ; DAC
           (error "Arithmetic coding currently unsupported."))
          ((#xffdd)                     ; DRI
           (let ((len (read-u16 port)))
             (unless (= len 4)
               (error "Unexpected DRI len" len))
             (set! restart-interval (read-u16 port))
             (lp)))
          ((#xfffe                      ; COM
            #xffe0 #xffe1 #xffe2 #xffe3 #xffe4 #xffe5 #xffe6 #xffe7 ; APP0-APP7
            #xffe8 #xffe9 #xffea #xffeb #xffec #xffed #xffee #xffef) ; APP8-APP15
           (let* ((len (read-u16 port))
                  (payload-len (- len 2)))
             (unless (>= payload-len 0)
               (error "Invalid comment/app segment length" marker len))
             (let ((misc (make-misc marker (read-bytes port payload-len))))
               (set! misc-segments (cons misc misc-segments))
               (lp))))
          (else
           (values (make-params q-tables dc-tables ac-tables restart-interval
                                (reverse misc-segments))
                   marker)))))))

(define (skip-params port)
  (let ((marker (read-marker port)))
    (case marker
      ((#xffdb ; DQT
        #xffc4 ; DHT
        #xffcc ; DAC
        #xffdd ; DRI
        #xfffe ; COM
        #xffe0 #xffe1 #xffe2 #xffe3 #xffe4 #xffe5 #xffe6 #xffe7 ; APP0-APP7
        #xffe8 #xffe9 #xffea #xffeb #xffec #xffed #xffee #xffef) ; APP8-APP15
       (let* ((len (read-u16 port))
              (payload-len (- len 2)))
         (unless (>= payload-len 0)
           (error "Invalid marker segment length" marker len))
         (seek port payload-len SEEK_CUR)
         (skip-params port)))
      (else marker))))

(define-record-type <frame>
  (make-frame marker precision y x components samp-x samp-y)
  frame?
  (marker frame-marker)
  (precision frame-precision)
  (y frame-y)
  (x frame-x)
  (components frame-components)
  (samp-x frame-samp-x)
  (samp-y frame-samp-y))

(define (frame-baseline? frame)
  (case (frame-marker frame)
    ((#xffc0) #t)                       ; SOF0
    (else #f)))

(define (frame-sequential? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc3 #xffc9 #xffcb) #t) ; SOF0,SOF1,SOF3,SOF9,SOF11
    (else #f)))

(define (frame-progressive? frame)
  (case (frame-marker frame)
    ((#xffc2 #xffca) #t)                ; SOF2,SOF10
    (else #f)))

(define (frame-huffman-coded? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc2 #xffc3) #t)  ; SOF0,SOF1,SOF2,SOF3
    (else #f)))

(define (frame-arithmetic-coded? frame)
  (case (frame-marker frame)
    ((#xffc9 #xffca #xffcb) #t)         ; SOF9,SOF10,SOF11
    (else #f)))

(define (frame-lossless? frame)
  (case (frame-marker frame)
    ((#xffc3 #xffcb) #t)                ; SOF3,SOF11
    (else #f)))

(define (frame-dct? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc2 #xffc9 #xffca) #t) ; SOF0,SOF1,SOF2,SOF9,SOF10
    (else #f)))

(define (frame-component-count frame)
  (vector-length (frame-components frame)))

(define (frame-mcu-width frame)
  (ceiling/ (frame-x frame) (* (frame-samp-x frame) 8)))

(define (frame-mcu-height frame)
  (ceiling/ (frame-y frame) (* (frame-samp-y frame) 8)))

(define-record-type <component>
  (make-component id index samp-x samp-y q-table)
  component?
  (id component-id)
  (index component-index)
  (samp-x component-samp-x)
  (samp-y component-samp-y)
  (q-table component-q-table))

(define* (vector-fold* f seed v #:key (key identity))
  (vector-fold (lambda (i seed elt) (f (key elt) seed)) seed v))

(define (read-frame-header port sof)
  (case sof
    ;; There is no SOF8.
    ((#xffc0 #xffc1 #xffc2 #xffc3 #xffc4 #xffc5 #xffc6 #xffc7 ; SOF0-SOF7
             #xffc9 #xffca #xffcb #xffcc #xffcd #xffce #xffcf) ; SOF9-SOF15
     (let* ((len (read-u16 port)))
       (unless (>= len 8)
         (error "Invalid frame header segment length" sof len))
       (let* ((precision (read-u8 port))
              (y (read-u16 port))
              (x (read-u16 port))
              (component-count (read-u8 port)))
         (unless (= len (+ 8 (* component-count 3)))
           (error "Invalid frame header segment length" sof len))
         (unless (> component-count 0)
           (error "No components in frame"))
         (when (zero? x)
           (error "Invalid zero-width image"))
         (when (zero? y)
           (error "DNL not supported"))
         (let* ((components
                 (vector-unfold
                  (lambda (n)
                    (let* ((id (read-u8 port))
                           (samp (read-u8 port))
                           (samp-x (ash samp -4))
                           (samp-y (logand samp #xf))
                           (table (read-u8 port)))
                      ;; Although 3 is technically permitted, it's
                      ;; pretty bogus.
                      (unless (memv samp-x '(1 2 4))
                        (error "Bad horizontal sampling value" samp-x))
                      (unless (memv samp-x '(1 2 4))
                        (error "Bad vertical sampling value" samp-y))
                      (unless (< table 4)
                        (error "Bad quantization table value" table))
                      (make-component id n samp-x samp-y table)))
                  component-count))
                (samp-x (vector-fold* max 1 components #:key component-samp-x))
                (samp-y (vector-fold* max 1 components #:key component-samp-y)))
           (make-frame sof precision y x components samp-x samp-y)))))
    (else (error "Invalid start-of-frame marker" sof))))

(define (allocate-dct-matrix frame)
  (unless (= 8 (frame-precision frame))
    (error "12-bit precision not supported"))
  (array-unfold
   (lambda (i j)
     (vector-map
      (lambda (i component)
        (array-unfold
         (lambda (i j)
           (make-vector (* 8 8) 0))
         (list (component-samp-y component) (component-samp-x component))))
      (frame-components frame)))
   (list (frame-mcu-height frame) (frame-mcu-width frame))))

(define (make-bit-port port)
  ;; Bit count, values, and the port
  (vector 0 0 port))

(define-inlinable (read-bits bit-port n)
  (match bit-port
    (#(count bits port)
     (let lp ((count count) (bits bits))
       (cond
        ((<= n count)
         (vector-set! bit-port 0 (- count n))
         (logand (ash bits (- n count)) (1- (ash 1 n))))
        (else
         (let* ((u8 (read-u8 port))
                ;; We never need more than 16 bits in the buffer.
                (bits (+ (logand (ash bits 8) #xffff) u8)))
           (when (= u8 #xff)
             (unless (zero? (read-u8 port))
               (error "Found marker while reading bits")))
           (vector-set! bit-port 1 bits)
           (lp (+ count 8) bits))))))))

(define (read-bit bit-port)
  (read-bits bit-port 1))

(define (read-signed-bits bit-port n)
  (let ((bits (read-bits bit-port n)))
    (if (< bits (ash 1 (1- n)))
        (+ (ash -1 n) 1 bits)
        bits)))

(define (read-huffman-value bit-port table)
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

;; return current dc
(define (read-block bit-port block prev-dc-q q-table dc-table ac-table)
  (define (record! index quantized-coefficient)
    (let* ((index (bytevector-u8-ref normal-order index))
           (q (vector-ref q-table index)))
      (vector-set! block index (* quantized-coefficient q))))
  ;; First, read DC coefficient.
  (let* ((dc-diff-bits (read-huffman-value bit-port dc-table))
         (dc-qdiff (read-signed-bits bit-port dc-diff-bits))
         (dc-q (+ prev-dc-q dc-qdiff)))
    (record! 0 dc-q)
    ;; Now read AC coefficients.
    (let lp ((k 1))
      (let* ((code (read-huffman-value bit-port ac-table)))
        (let ((r (ash code -4))
              (s (logand code #xf)))
          (cond
           ((zero? s)
            ;; #xf0 indicates 16 zeroes.  Otherwise stop.
            (when (eqv? r #xf)
              (lp (+ k 16))))
           (else
            (let* ((bits (read-signed-bits bit-port s))
                   (k (+ k r)))
              (record! k bits)
              ;; Loop if there are more coefficients.
              (when (< k 63)
                (lp (1+ k)))))))))
    ;; Return DC coefficient.
    dc-q))

(define (read-mcu bit-port scan-components mcu)
  (vector-for-each
   (lambda (k scan-component)
     (match scan-component
       (#(component prev-dc q-table dc-table ac-table)
        (let ((dc (array-fold-values
                   (lambda (block prev-dc)
                     (read-block bit-port block
                                 prev-dc q-table dc-table ac-table))
                   (vector-ref mcu (component-index component))
                   prev-dc)))
          (vector-set! scan-component 1 dc)))))
   scan-components))

(define (read-dct-scan bit-port scan-components dest Ss Se Ah Al)
  (unless (and (= Ss 0) (= Se 63) (= Ah 0) (= Al 0))
    (error "progressive frame reading not yet supported"))
  (array-for-each-value (lambda (mcu)
                          (read-mcu bit-port scan-components mcu))
                        dest))

(define (read-scan port frame params dest)
  (define (find-component id)
    (let ((components (frame-components frame)))
      (vector-ref components
                  (or (vector-index (lambda (component)
                                      (= (component-id component) id))
                                    components)
                      (error "No component found with id" id)))))
  (unless (frame-dct? frame) (error "DCT frame expected" frame))
  (unless (frame-huffman-coded? frame) (error "Huffman coding expected" frame))
  (let ((len (read-u16 port)))
    (unless (>= len 6)
      (error "Unexpected scan segment length" len))
    (let ((scan-component-count (read-u8 port)))
      (unless (= len (+ 6 (* scan-component-count 2)))
        (error "Unexpected scan segment length" len))
      (let* ((scan-components
              (vector-unfold
               (lambda (i next-component-index)
                 (let* ((id (read-u8 port))
                        (T (read-u8 port))
                        (Td (ash T -4))
                        (Ta (logand T #xf))
                        (component (find-component id)))
                   (unless (< Td 4) (error "Bad Td" Td))
                   (unless (< Ta 4) (error "Bad Ta" Ta))
                   (unless (<= (component-index component) next-component-index)
                     (error "Bad component ordering in scan" component))
                   (values
                    (vector component
                            0 ;; Previous DC coefficient.
                            (let ((q (component-q-table component)))
                              (or (vector-ref (q-tables params) q)
                                  (error "Missing Q table" q)))
                            (or (vector-ref (dc-tables params) Td)
                                (error "Missing DC table" Td))
                            (or (vector-ref (ac-tables params) Ta)
                                (error "Missing AC table" Ta)))
                    (1+ (component-index component)))))
               scan-component-count
               0))
             (Ss (read-u8 port))
             (Se (read-u8 port))
             (A (read-u8 port))
             (Ah (ash A -4))
             (Al (logand A #xf))
             (bit-port (make-bit-port port)))
        (cond
         ((frame-sequential? frame)
          (unless (zero? Ss) (error "Bad Ss for sequential frame" Ss))
          (unless (= Se 63) (error "Bad Se for sequential frame" Se))
          (unless (zero? Ah) (error "Bad Ah for sequential frame" Ah))
          (unless (zero? Al) (error "Bad Al for sequential frame" Al))
          (read-dct-scan bit-port scan-components dest 0 63 0 0))
         ((frame-progressive? frame)
          (unless (<= Ss Se 63) (error "Bad Ss / Se" Ss Se))
          (unless (< Ah 14) (error "Bad Ah" Ah))
          (unless (< Al 14) (error "Bad Ah" Al))
          (read-dct-scan bit-port scan-components dest Ss Se Ah Al))
         (else (error "Unsupported frame type" frame)))))))

(define (jpeg-dimensions file)
  (let ((port (open-input-file file)))
    (read-soi port)
    (let ((header (read-frame-header port (skip-params port))))
      (values (frame-x header)
              (frame-y header)))))

(define (find-exif params)
  (define (bv-prefix? prefix bv)
    (and (>= (bytevector-length bv) (bytevector-length prefix))
         (let lp ((n 0))
           (or (= n (bytevector-length prefix))
               (and (eqv? (bytevector-u8-ref prefix n)
                          (bytevector-u8-ref bv n))
                    (lp (1+ n)))))))
  (define (bv-suffix bv start)
    (let* ((len (- (bytevector-length bv) start))
           (out (make-bytevector len)))
      (bytevector-copy! bv start out 0 len)
      out))
  (filter-map (lambda (misc)
                (and (= (misc-marker misc) #xffe1) ; APP1
                     (bv-prefix? #vu8(69 120 105 102 0 0) (misc-bytes misc))
                     (parse-exif (bv-suffix (misc-bytes misc) 6))))
              (misc-segments params)))

(define (jpeg-dimensions-and-exif file)
  (let ((port (open-input-file file)))
    (read-soi port)
    (call-with-values (lambda () (read-params port))
      (lambda (params sof)
        (let ((header (read-frame-header port sof)))
          (values (frame-x header)
                  (frame-y header)
                  (match (find-exif params)
                    (((main thumbnail)) main)
                    (((main)) main)
                    (_ '()))))))))

(define (parse-jpeg file)
  (let ((port (open-input-file file)))
    (read-soi port)
    (call-with-values (lambda () (read-params port))
      (lambda (image-params sof)
        (let* ((frame (read-frame-header port sof))
               (dest (allocate-dct-matrix frame)))
          (list frame
                (misc-segments image-params)
                (let lp ((params image-params))
                  (call-with-values (lambda () (read-params port params))
                    (lambda (scan-params marker)
                      (case marker
                        ((#xffd9)       ; EOI
                         dest)
                        ((#xffda)       ; SOS
                         (read-scan port frame scan-params dest)
                         (lp scan-params))
                        (else
                         (error "Unexpected marker" marker))))))))))))

(define fdct-coefficients
  (eval-at-compile-time
   (let ((pi (* 2 (acos 0))))
     (array-unfold
      (lambda (u v)
        (typed-array-unfold
         (lambda (k)
           (call-with-values (lambda () (euclidean/ k 8))
             (lambda (i j)
               (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                     (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                 (* 1/4 Cu Cv
                    (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                    (cos (/ (* (+ (* 2 j) 1) v pi) 16)))))))
         'f32 (list (* 8 8))))
      (list 8 8)))))

(define idct-coefficients
  (eval-at-compile-time
   (let ((pi (* 2 (acos 0))))
     (array-unfold
      (lambda (i j)
        (typed-array-unfold
         (lambda (k)
           (call-with-values (lambda () (euclidean/ k 8))
             (lambda (u v)
               (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                     (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                 (* 1/4 Cu Cv
                    (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                    (cos (/ (* (+ (* 2 j) 1) v pi) 16)))))))
         'f32 (list (* 8 8))))
      (list 8 8)))))

(define (idct-block block plane pos stride)
  (define (idct i j)
    (array-fold
     (lambda (k coeff sum)
       (+ sum (* coeff (vector-ref block k))))
     (array-ref idct-coefficients i j)
     0.0))
  (let lp ((i 0) (pos pos))
    (when (< i 8)
      (let lp ((j 0))
        (when (< j 8)
          (let* ((s (idct i j))
                 (sq (cond
                      ((< s -128.0) 0)
                      ((> s 127.0) 255)
                      (else (+ 128 (inexact->exact (round s)))))))
            (bytevector-u8-set! plane (+ pos j) sq))
          (lp (1+ j))))
      (lp (1+ i) (+ pos stride)))))

(define-record-type <yuv>
  (make-yuv width height canvas-width canvas-height planes)
  yuv?
  (width yuv-width)
  (height yuv-height)
  (canvas-width yuv-canvas-width)
  (canvas-height yuv-canvas-height)
  (planes yuv-planes))

(define-record-type <plane>
  (make-plane width height samples)
  plane?
  (width plane-width)
  (height plane-height)
  (samples plane-samples))

;; It's really Y' Cb Cr.  Don't tell Poynton.
(define (decode-jpeg-to-yuv parsed)
  (match parsed
    ((frame misc-segments mcu-array)
     (let ((mcu-width (frame-mcu-width frame))
           (mcu-height (frame-mcu-height frame)))
       (make-yuv
        (frame-x frame)
        (frame-y frame)
        (* mcu-width (frame-samp-x frame) 8)
        (* mcu-height (frame-samp-y frame) 8)
        (vector-map
         (lambda (k component)
           (let* ((samp-x (component-samp-x component))
                  (samp-y (component-samp-y component))
                  (block-width (* mcu-width samp-x))
                  (block-height (* mcu-height samp-y))
                  (sample-width (* block-width 8))
                  (sample-height (* block-height 8))
                  (plane (make-bytevector (* sample-width sample-height) 0)))
             (array-for-each
              (lambda (i j mcu)
                (let* ((mcu-y (* i samp-y 8))
                       (mcu-x  (* j samp-x 8))
                       (offset (+ (* mcu-y sample-width) mcu-x)))
                  (array-for-each
                   (lambda (i j block)
                     (let ((offset (+ offset (* i 8 sample-width) (* j 8))))
                       (idct-block block plane offset sample-width)))
                   (vector-ref mcu k))))
              mcu-array)
             (make-plane sample-width sample-height plane)))
         (frame-components frame)))))))

(define (expand-width-by-two/centered in width height)
  (let* ((out (make-bytevector (* width 2 height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i width))
              (out-pos (* i width 2)))
          ;; Special case for first column.
          (let* ((j 0)
                 (in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos 0) in))
          (let lp ((j 0))
            (when (< j (1- width))
              ;; (3x + y + 2) >> 2 is the same as 3x/4 + y/4.  Since
              ;; we're dealing with integers though, we don't want to
              ;; introduce bias by having all 0.5 values round to 1, so
              ;; we add 1 or 2 to the value being shifted, alternating
              ;; by row.
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos (1+ j))))
                     (out- (ash (+ (* 3 in-) in+ 2) -2))
                     (out+ (ash (+ in- (* 3 in+) 1) -2)))
                (bytevector-u8-set! out (+ out-pos j j 1) out-)
                (bytevector-u8-set! out (+ out-pos j j 2) out+)
                (lp (+ j 1)))))
          ;; Special case for last column.
          (let* ((j (1- width))
                 (in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos width width -1) in)))
        (lp (1+ i))))
    out))

(define (expand-height-by-two/centered in width height)
  (let* ((out (make-bytevector (* width 2 height) 0)))
    ;; Special case for first row.
    (let lp ((j 0))
      (when (< j width)
        (let ((in (bytevector-u8-ref in j)))
          (bytevector-u8-set! out j in)
          (lp (1+ j)))))
    ;; The height-1 spaces between samples.
    (let lp ((i 0))
      (when (< i (1- height))
        (let ((in-pos (* i width))
              (out-pos (+ width (* i 2 width))))
          (let lp ((j 0))
            (when (< j width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos width j)))
                     ;; Interpolate output; see comment in previous
                     ;; function.
                     (out- (ash (+ (* 3 in-) in+ 2) -2))
                     (out+ (ash (+ in- (* 3 in+) 1) -2)))
                (bytevector-u8-set! out (+ out-pos j) out-)
                (bytevector-u8-set! out (+ out-pos width j) out+)
                (lp (1+ j)))))
          (lp (1+ i)))))
    ;; Special case for the last row.
    (let* ((i (1- height))
           (in-pos (* i width))
           (out-pos (+ width (* i 2 width))))
      (let lp ((j 0))
        (when (< j width)
          (let ((in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos j) in)
            (lp (1+ j))))))
    out))

(define (upsample-4:2:2 width height y-width y-height y cb cr)
  (define (expand in)
    (expand-width-by-two/centered in (/ y-width 2) y-height))
  (make-yuv width height y-width y-height
            (vector (make-plane y-width y-height y)
                    (make-plane y-width y-height (expand cb))
                    (make-plane y-width y-height (expand cr)))))

(define (upsample-4:2:0 width height y-width y-height y cb cr)
  (define (expand in)
    (expand-height-by-two/centered in (/ y-width 2) (/ y-height 2)))
  (upsample-4:2:2 width height y-width y-height y (expand cb) (expand cr)))

(define (convert-yuv out width height stride y cb cr y-stride)
  (let lp ((i 0))
    (when (< i height)
      (let lp ((j 0) (in-pos (* i y-stride)) (out-pos (* i stride)))
        (when (< j width)
          (let ((y (bytevector-u8-ref y in-pos))
                (cb (- (bytevector-u8-ref cb in-pos) 128))
                (cr (- (bytevector-u8-ref cr in-pos) 128)))
            (define (->u8 x)
              (cond ((< x 0) 0)
                    ((> x 255) 255)
                    (else (inexact->exact (round x)))))
            ;; See ITU recommendataion ITU-T T.871, "JPEG File
            ;; Interchange Format (JFIF)", section 7.
            (let ((r (->u8 (+ y (* 1.402 cr))))
                  (g (->u8 (- y (/ (+ (* 0.114 1.772 cb)
                                      (* 0.299 1.402 cr))
                                   0.587))))
                  (b (->u8 (+ y (* 1.772 cb)))))
              (bytevector-u8-set! out (+ out-pos 0) r)
              (bytevector-u8-set! out (+ out-pos 1) g)
              (bytevector-u8-set! out (+ out-pos 2) b)
              (lp (1+ j) (1+ in-pos) (+ out-pos 3))))))
      (lp (1+ i)))))

(define* (yuv->rgb yuv #:optional (stride (* (yuv-width yuv) 3)))
  (match yuv
    (($ <yuv> width height canvas-width canvas-height planes)
     (match planes
       (#(($ <plane> y-width y-height y))
        (error "greyscale unimplemented"))
       (#(($ <plane> y-width y-height y)
          ($ <plane> cb-width cb-height cb)
          ($ <plane> cr-width cr-height cr))
        (unless (and (= y-width canvas-width) (= y-height canvas-height))
          (error "Expected Y' to have same dimensions as canvas"))
        (let ((rgb (make-bytevector (* height stride) 0)))
          (match (vector (/ y-width cb-width) (/ y-height cb-height)
                         (/ y-width cr-width) (/ y-height cr-height))
            (#(2 2 2 2)                 ; 4:2:0
             (yuv->rgb (upsample-4:2:0 width height y-width y-height y cb cr)
                       stride))
            (#(2 1 2 1) ; 4:2:2
             (yuv->rgb (upsample-4:2:2 width height y-width y-height y cb cr)
                       stride))
            (#(1 1 1 1) ; 4:4:4
             (unless (<= (* width 3) stride)
               (error "invalid stride" stride))
             (let ((out (make-bytevector (* stride height) 0)))
               (convert-yuv out width height stride y cb cr y-width)
               out))
            (#(x y z w) ; ?
             (error "subsampling unimplemented" x y z w)))))
       (_ (error "unknown colorspace"))))))

(define (pad-rgb-horizontally rgb width height stride new-width)
  (let* ((new-stride (* new-width 3))
         (out (make-bytevector (* new-stride height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! rgb in-pos out out-pos (* width 3))
          (let lp ((j (* width 3)))
            (when (< j new-stride)
              (let ((x (bytevector-u8-ref out (+ out-pos j -3))))
                (bytevector-u8-set! out (+ out-pos j) x)
                (lp (1+ j))))))
        (lp (1+ i))))
    out))

(define (pad-rgb-vertically rgb width height stride new-height)
  (let* ((new-stride (* width 3))
         (out (make-bytevector (* new-stride new-height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! rgb in-pos out out-pos (* width 3))
          (lp (1+ i)))))
    (let lp ((i height))
      (when (< i new-height)
        (let ((prev-pos (* (1- i) new-stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! out prev-pos out out-pos new-stride)
          (lp (1+ i)))))
    out))

(define (shrink-width-by-two/centered in width height)
  (let* ((half-width (/ width 2))
         (out (make-bytevector (* half-width height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i width))
              (out-pos (* i half-width)))
          (let lp ((j 0))
            (when (< j half-width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos (* j 2))))
                     (in+ (bytevector-u8-ref in (+ in-pos (* j 2) 1)))
                     ;; Dither rounding alternately by column.
                     (out* (ash (+ in- in+ (logand j 1)) -1)))
                (bytevector-u8-set! out (+ out-pos j) out*)
                (lp (1+ j))))))
        (lp (1+ i))))
    out))

(define (shrink-height-by-two/centered in width height)
  (let* ((half-height (/ height 2))
         (out (make-bytevector (* width half-height) 0)))
    (let lp ((i 0))
      (when (< i half-height)
        (let ((in-pos (* i 2 width))
              (out-pos (* i width)))
          (let lp ((j 0))
            (when (< j width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos j width)))
                     ;; Dither rounding alternately by column.
                     (out* (ash (+ in- in+ (logand j 1)) -1)))
                (bytevector-u8-set! out (+ out-pos j) out*)
                (lp (1+ j))))))
        (lp (1+ i))))
    out))

(define (convert-rgb rgb width height stride)
  (let ((y (make-bytevector (* width height)))
        (cb (make-bytevector (* width height)))
        (cr (make-bytevector (* width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0) (in-pos (* i stride)) (out-pos (* i width)))
          (when (< j width)
            (let ((r (bytevector-u8-ref rgb (+ in-pos 0)))
                  (g (bytevector-u8-ref rgb (+ in-pos 1)))
                  (b (bytevector-u8-ref rgb (+ in-pos 2))))
              (define (->u8 x)
                (cond ((< x 0) 0)
                      ((> x 255) 255)
                      (else (inexact->exact (round x)))))
              ;; See ITU recommendataion ITU-T T.871, "JPEG File
              ;; Interchange Format (JFIF)", section 7.
              (let ((y* (->u8 (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
                    (cb* (->u8 (+ (/ (+ (* -0.299 r) (* -0.587 g) (* 0.886 b))
                                     1.772)
                                  128)))
                    (cr* (->u8 (+ (/ (+ (* 0.701 r) (* -0.587 g) (* -0.114 b))
                                     1.402)
                                  128))))
                (bytevector-u8-set! y out-pos y*)
                (bytevector-u8-set! cb out-pos cb*)
                (bytevector-u8-set! cr out-pos cr*)
                (lp (1+ j) (+ in-pos 3) (1+ out-pos))))))
        (lp (1+ i))))
    (values y cb cr)))

(define* (rgb->yuv rgb width height #:key (stride (* width 3))
                   (samp-x 2) (samp-y 2))
  (define (round-up x y) (* (ceiling/ x y) y))
  (let pad ((rgb rgb)
            (canvas-width width)
            (canvas-height height)
            (stride stride))
    (cond
     ((not (integer? (/ canvas-width 8 samp-x)))
      (let ((new-canvas-width (round-up canvas-width (* 8 samp-x))))
        (pad (pad-rgb-horizontally rgb canvas-width canvas-height stride
                                   new-canvas-width)
             new-canvas-width canvas-height (* new-canvas-width 3))))
     ((not (integer? (/ canvas-height 8 samp-y)))
      (let ((new-canvas-height (round-up canvas-height (* 8 samp-y))))
        (pad (pad-rgb-vertically rgb canvas-width canvas-height stride
                                 new-canvas-height)
             canvas-width new-canvas-height (* canvas-width 3))))
     (else
      (call-with-values (lambda ()
                          (convert-rgb rgb canvas-width canvas-height stride))
        (lambda (y cb cr)
          (let lp ((cb cb) (cr cr)
                   (samp-width canvas-width) (samp-height canvas-height))
            (cond
             ((< canvas-width (* samp-width samp-x))
              (lp (shrink-width-by-two/centered cb samp-width samp-height)
                  (shrink-width-by-two/centered cr samp-width samp-height)
                  (/ samp-width 2)
                  samp-height))
             ((< canvas-height (* samp-height samp-y))
              (lp (shrink-height-by-two/centered cb samp-width samp-height)
                  (shrink-height-by-two/centered cr samp-width samp-height)
                  samp-width
                  (/ samp-height 2)))
             (else
              (make-yuv width height canvas-width canvas-height
                        (vector
                         (make-plane canvas-width canvas-height y)
                         (make-plane samp-width samp-height cb)
                         (make-plane samp-width samp-height cr))))))))))))

(define (write-ppm port rgb width height)
  (format port "P6\n~a ~a\n255\n" width height)
  (put-bytevector port rgb))

(define (jpeg->ppm in out)
  (let ((yuv (decode-jpeg-to-yuv (parse-jpeg in))))
    (write-ppm (open-output-file out)
               (yuv->rgb yuv)
               (yuv-width yuv)
               (yuv-height yuv))))

(define (write-pgm port y width height)
  (format port "P5\n~a ~a\n255\n" width height)
  (put-bytevector port y))

(define (jpeg-plane->pgm in out idx)
  (let* ((yuv (decode-jpeg-to-yuv (parse-jpeg in)))
         (plane (vector-ref (yuv-planes yuv) idx)))
    (write-pgm (open-output-file out)
               (plane-samples plane)
               (plane-width plane)
               (plane-height plane))))

;; Tables K.1 and K.2 from the JPEG specification.
(define *standard-luminance-q-table*
  #(16 11 10 16 24 40 51 61
    12 12 14 19 26 58 60 55
    14 13 16 24 40 57 69 56
    14 17 22 29 51 87 80 62
    18 22 37 56 68 109 103 77
    24 35 55 64 81 104 113 92
    49 64 78 87 103 121 120 101
    72 92 95 98 112 100 103 99))

(define *standard-chrominance-q-table*
  #(17 18 24 47 99 99 99 99
    18 21 26 66 99 99 99 99
    24 26 56 99 99 99 99 99
    47 66 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99))

;; As libjpeg does, we consider the above tables to be quality 50, on a
;; scale from 1 (terrible) to 100 (great).  We linearly scale the values
;; so that at quality 100, all values are 1, and at quality 1 all values
;; are 255.
(define (q-tables-for-quality quality)
  ;; This mapping of quality to a linear scale is also from libjpeg.
  (let* ((quality (exact->inexact quality)) ;; allow divide by zero -> inf
         (linear-scale (if (< quality 50)
                           (/ 50. quality)
                           (- 1 (/ (- quality 50) 50)))))
    (define (scale x)
      (let ((x (* x linear-scale)))
        (cond
         ((< x 1) 1)
         ((> x 255) 255)
         (else (inexact->exact (round x))))))
    (vector (array-map-values scale *standard-luminance-q-table*)
            (array-map-values scale *standard-chrominance-q-table*)
            #f
            #f)))

(define (q-tables-for-mcu-array mcu-array)
  (define (gcd* coeff q) (gcd (abs coeff) q))
  (define (meet-tables coeffs q)
    (if q
        (array-map-values gcd* coeffs q)
        (array-map-values abs coeffs)))
  (call-with-values
      (lambda ()
        (array-fold-values
         (lambda (mcu luma-q chroma-q)
           (match mcu
             (#(y)
              (values (array-fold-values meet-tables y luma-q) chroma-q))
             (#(y u v)
              (values (array-fold-values meet-tables y luma-q)
                      (let ((q (array-fold-values meet-tables u chroma-q)))
                        (array-fold-values meet-tables v q))))))
         mcu-array #f #f))
    (lambda (luma-q chroma-q)
      (define (fixup q) (if (zero? q) 255 q))
      (vector (array-map-values fixup luma-q) (array-map-values fixup chroma-q)
              #f #f))))

(define (fdct-block plane pos stride q-table)
  (define (fdct v u)
    (let ((coeffs (array-ref fdct-coefficients v u)))
      (let lp ((i 0) (pos pos) (sum 0.0))
        (if (< i 8)
            (lp (1+ i)
                (+ pos stride)
                (let lp ((j 0) (k (* i 8)) (pos pos) (sum sum))
                  (if (< j 8)
                      (let ((coeff (f32vector-ref coeffs k))
                            (sample (- (bytevector-u8-ref plane pos) 128)))
                        (lp (1+ j)
                            (1+ k)
                            (1+ pos)
                            (+ sum (* coeff sample))))
                      sum)))
            sum))))
  (vector-unfold
   (lambda (k)
     (let ((v (ash k -3))
           (u (logand k 7))
           (q (vector-ref q-table k)))
       (let ((Svu (fdct v u)))
         (* q (inexact->exact (round (/ Svu q)))))))
   (* 8 8)))

(define* (fdct-frame yuv #:key (quality 85)
                     (q-tables (q-tables-for-quality quality))
                     ;; Table 0 is for luminance, and 1 is for other
                     ;; components.
                     (plane-q-table (lambda (i) (if (zero? i) 0 1))))
  (match yuv
    (($ <yuv> width height canvas-width canvas-height planes)
     (define (x-subsampling plane) (/ canvas-width (plane-width plane)))
     (define (y-subsampling plane) (/ canvas-height (plane-height plane)))
     (let ((samp-x (vector-fold* lcm 1 planes #:key x-subsampling))
           (samp-y (vector-fold* lcm 1 planes #:key y-subsampling)))
       (define (plane-samp-x plane)
         (* samp-x (/ (plane-width plane) canvas-width)))
       (define (plane-samp-y plane)
         (* samp-y (/ (plane-height plane) canvas-height)))
       (let ((components
              (array-unfold
               (lambda (i)
                 (let* ((plane (vector-ref planes i))
                        (samp-x (plane-samp-x plane))
                        (samp-y (plane-samp-y plane)))
                   (make-component i i samp-x samp-y (plane-q-table i))))
               (array-dimensions planes))))
         (list
          (make-frame #f 8 height width components samp-x samp-y)
          '()
          (array-unfold
           (lambda (i j)
             (array-map-values
              (lambda (component)
                (match (vector-ref planes (component-index component))
                  (($ <plane> plane-width plane-height samples)
                   (let ((samp-y (component-samp-y component))
                         (samp-x (component-samp-x component)))
                     (array-unfold
                      (lambda (y x)
                        (let* ((pos (+ (* (+ (* i samp-y) y) 8 plane-width)
                                       (* (+ (* j samp-x) x) 8)))
                               (q-table-index (component-q-table component))
                               (q-table (vector-ref q-tables q-table-index)))
                          (fdct-block samples pos plane-width q-table)))
                      (list samp-y samp-x))))))
              components))
           (list (/ canvas-height 8 samp-y) (/ canvas-width 8 samp-x)))))))))

(define (encode-block block q-table prev-dc)
  (let ((zzq (vector-unfold
              (lambda (i)
                (let ((i (bytevector-u8-ref normal-order i)))
                  (/ (vector-ref block i) (vector-ref q-table i))))
              64)))
    (define (bit-count x)
      (cond
       ((negative? x) (let lp ((n 1)) (if (< (ash -1 n) x) n (lp (1+ n)))))
       ((zero? x) 0)
       (else  (let lp ((n 1)) (if (< x (ash 1 n)) n (lp (1+ n)))))))
    (define (code-and-bits code bits) (logior code (ash bits 8)))
    (define (encode-dc dc) (code-and-bits (bit-count dc) dc))
    (define (encode-ac ac zero-count)
      (code-and-bits (logior (ash zero-count 4) (bit-count ac)) ac))
    (define (skip-zeroes i zero-count codes)
      (let ((ac (vector-ref zzq i)))
        (if (zero? ac)
            (if (= i 63)
                (cons 0 codes) ;; EOB.
                (skip-zeroes (1+ i) (1+ zero-count) codes))
            (let lp ((zero-count zero-count) (codes codes))
              (if (< zero-count 16)
                  (encode-next (1+ i)
                               (cons (encode-ac ac zero-count) codes))
                  (lp (- zero-count 16) (cons #xf0 codes))))))) ; ZRL.
    (define (encode-next i codes)
      (if (= i 64)
          codes
          (skip-zeroes i 0 codes)))
    (let ((dc (vector-ref zzq 0)))
      (values dc
              (cons (encode-dc (- dc prev-dc))
                    (reverse (encode-next 1 '())))))))

(define (encode-frame parsed q-tables)
  (define (compute-scan-components frame mcu-array)
    (array-map-values
     (lambda (component)
       (let ((q-table (vector-ref q-tables (component-q-table component))))
         ;; We don't know the dc and ac huffman tables yet.
         (vector component 0 q-table #f #f)))
     (frame-components frame)))
  (match parsed
    ((frame misc mcu-array)
     (let ((scan-components (compute-scan-components frame mcu-array)))
       (array-map-values
        (lambda (mcu)
          (array-map-values
           (lambda (blocks scan-component)
             (match scan-component
               (#(component prev-dc q-table dc-table ac-table)
                (call-with-values
                    (lambda ()
                      (array-fold-values
                       (lambda (block dc out)
                         (call-with-values (lambda ()
                                             (encode-block block q-table dc))
                           (lambda (dc codes)
                             (values dc (cons codes out)))))
                       blocks prev-dc '()))
                  (lambda (dc out)
                    (vector-set! scan-component 1 dc)
                    (reverse out))))))
           mcu
           scan-components))
        mcu-array)))))

(define (compute-code-frequencies codes)
  (let ((dc-freqs (vector (make-vector 256 0) (make-vector 256 0) #f #f))
        (ac-freqs (vector (make-vector 256 0) (make-vector 256 0) #f #f)))
    (define (count! table code)
      (let ((idx (logand code #xff)))
        (vector-set! table idx (1+ (vector-ref table idx)))))
    (define (accumulate-frequencies codes idx)
      (let ((dc-freqs (vector-ref dc-freqs idx))
            (ac-freqs (vector-ref ac-freqs idx)))
        (match codes
          ((dc . ac)
           (count! dc-freqs dc)
           (for-each (lambda (ac) (count! ac-freqs ac)) ac)))))
    (array-for-each-value
     (lambda (mcu)
       (vector-for-each
        (lambda (k blocks)
          (for-each
           (lambda (codes)
             (let ((idx (if (zero? k) 0 1)))
               (accumulate-frequencies codes idx)))
           blocks))
        mcu))
     codes)
    (vector dc-freqs ac-freqs)))

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
                        (vector-copy! copy 0 freqs)
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
          (compute-huffman-codes (bytevector-truncate size-counts 16)
                                 values))))))

(define (compute-huffman-code-tables dc-and-ac-freqs)
  (array-map-values
   (lambda (freqs-v)
     (array-map-values
      (lambda (freqs)
        (and=> freqs compute-huffman-table-for-freqs))
      freqs-v))
   dc-and-ac-freqs))

(define (put-u16 port u16)
  (put-u8 port (ash u16 -8))
  (put-u8 port (logand u16 #xff)))

(define (write-soi port)
  (put-u16 port #xffd8)) ; SOI.

(define (write-misc-segment port misc)
  (put-u16 port (misc-marker misc))
  (put-u16 port (+ 2 (bytevector-length (misc-bytes misc))))
  (put-bytevector port (misc-bytes misc)))

(define (write-baseline-frame port frame)
  (put-u16 port #xffc0) ; SOF0.
  (let ((len (+ 8 (* (frame-component-count frame) 3))))
    (put-u16 port len))
  (put-u8 port (frame-precision frame))
  (put-u16 port (frame-y frame))
  (put-u16 port (frame-x frame))
  (put-u8 port (frame-component-count frame))
  (array-for-each-value
   (lambda (component)
     (put-u8 port (component-id component))
     (put-u8 port (logior (ash (component-samp-y component) 4)
                          (component-samp-x component)))
     (put-u8 port (component-q-table component)))
   (frame-components frame)))

(define (write-q-tables port q-tables)
  (vector-for-each
   (lambda (i table)
     (when table
       (put-u16 port #xffdb) ; DQT.
       (let ((len (+ 3 64)))
         (put-u16 port len))
       (let ((P 0)
             (T i))
         (put-u8 port (logior (ash P 4) T)))
       (let lp ((i 0))
         (when (< i 64)
           (let ((i (bytevector-u8-ref normal-order i)))
             (put-u8 port (vector-ref table i)))
           (lp (1+ i))))))
   q-tables))

(define (write-huffman-tables port huffman-tables)
  (define (write-table k table Tc)
    (match table
      (#f #f)
      (#(size-counts size-offsets
         values value-indexes sizes codes max-codes)
       (put-u16 port #xffc4)            ; DHT.
       (let ((len (+ 19 (bytevector-length values))))
         (put-u16 port len))
       (put-u8 port (logior (ash Tc 4) k))
       (put-bytevector port size-counts)
       (put-bytevector port values))))
  (match huffman-tables
    (#(dc-tables ac-tables)
     (vector-for-each (lambda (k table) (write-table k table 0))
                      dc-tables)
     (vector-for-each (lambda (k table) (write-table k table 1))
                      ac-tables))))

(define (write-baseline-scan-header port frame)
  (put-u16 port #xffda) ; SOS.
  (let ((len (+ 6 (* (frame-component-count frame) 2))))
    (put-u16 port len))
  (put-u8 port (frame-component-count frame))
  (vector-for-each
   (lambda (k component)
     (let ((Td (if (zero? k) 0 1))
           (Ta (if (zero? k) 0 1)))
       (put-u8 port (component-id component))
       (put-u8 port (logior (ash Td 4) Ta))))
   (frame-components frame))
  (let ((Ss 0)
        (Se 63)
        (Ah 0)
        (Al 0))
    (put-u8 port Ss)
    (put-u8 port Se)
    (put-u8 port (logior (ash Ah 4) Al))))

(define (put-u8/stuff port u8)
  (put-u8 port u8)
  (when (eqv? u8 #xff)
    (put-u8 port 0)))

(define (flush-bit-port bit-port)
  (match bit-port
    (#(count bits port)
     (unless (zero? count)
       ;; Pad remaining bits with 1, and stuff as needed.
       (let ((bits (logand #xff (logior (ash -1 count) bits))))
         (put-u8/stuff port bits))
       (vector-set! bit-port 0 0)
       (vector-set! bit-port 1 0)))))

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

(define (write-baseline-entropy-coded-data port codes huffman-tables)
  (let ((port (make-bit-port port)))
    (match huffman-tables
      (#(dc-tables ac-tables)
       (define (write-code code table)
         (match table
           (#(size-counts size-offsets
              values value-indexes sizes codes max-codes)
            (let* ((u8 (logand code #xff))
                   (diff (ash code -8))
                   (ssss (logand code #xf))
                   (code-index (vector-ref value-indexes u8))
                   (code (vector-ref codes code-index))
                   (size (bytevector-u8-ref sizes code-index)))
              (put-bits port code size)
              (unless (zero? ssss)
                (put-bits port diff ssss))))))
       (define (write-codes codes idx)
         (let ((dc-table (vector-ref dc-tables idx))
               (ac-table (vector-ref ac-tables idx)))
           (match codes
             ((dc . ac)
              (write-code dc dc-table)
              (for-each (lambda (ac) (write-code ac ac-table)) ac)))))
       (array-for-each-value
        (lambda (mcu)
          (vector-for-each
           (lambda (k blocks)
             (for-each
              (lambda (codes)
                (let ((idx (if (zero? k) 0 1)))
                  (write-codes codes idx)))
              blocks))
           mcu))
        codes)
       (flush-bit-port port)))))

(define (write-eoi port)
  (put-u16 port #xffd9)) ; EOI.

(define (write-jpeg port parsed)
  (match parsed
    ((frame misc mcu-array)
     (let* ((q-tables (q-tables-for-mcu-array mcu-array))
            (codes (encode-frame parsed q-tables))
            (frequencies (compute-code-frequencies codes))
            (huffman-tables (compute-huffman-code-tables frequencies)))
       (write-soi port)
       (for-each (lambda (misc) (write-misc-segment port misc)) misc)
       (write-baseline-frame port frame)
       (write-q-tables port q-tables)
       (write-huffman-tables port huffman-tables)
       (write-baseline-scan-header port frame)
       (write-baseline-entropy-coded-data port codes huffman-tables)
       (write-eoi port)))))
