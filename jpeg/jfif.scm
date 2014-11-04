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
;; Readers and writers for the JPEG File Interchange Format (JFIF)
;;
;;; Code:

(define-module (jpeg jfif)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold filter-map))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:use-module (rnrs bytevectors)
  #:use-module (jpeg array)
  #:use-module (jpeg bit-ports)
  #:use-module (jpeg huffman)
  #:use-module (jpeg pixbufs)
  #:export (<jpeg>
            make-jpeg jpeg?
            jpeg-frame jpeg-misc-segments jpeg-mcu-array

            <frame>
            make-frame frame?
            frame-marker frame-precision frame-y frame-x frame-components
            frame-samp-x frame-samp-y

            frame-baseline? frame-sequential? frame-progressive?
            frame-huffman-coded? frame-arithmetic-coded? frame-lossless?
            frame-dct?

            frame-component-count frame-mcu-width frame-mcu-height

            <component>
            make-component component?
            component-id component-index component-samp-x component-samp-y
            component-q-table

            <misc>
            make-misc misc?
            misc-marker misc-bytes

            read-jpeg
            write-jpeg))


;; See http://www.w3.org/Graphics/JPEG/itu-t81.pdf for ITU
;; recommendation T.81, which is a freely-available version of the JPEG
;; specification.

;; JPEG := SOI FRAME EOI
;; FRAME := MISC* FHEADER SCAN DNL? SCAN ... 
;; SCAN := MISC* SHEADER ECS (RST ECS)*
;; FHEADER := SOF LEN PRECISION Y X COMP0 COMP1 ...
;; MISC := (DQT | DHT | DAC | DRI | COM | APP) LEN payload...
;; SHEADER := SOS LEN NCOMPONENTS SCOMP0 SCOMP1 ... SS SE A

(define-record-type <jpeg>
  (make-jpeg frame misc-segments mcu-array)
  jpeg?
  (frame jpeg-frame)
  (misc-segments jpeg-misc-segments)
  (mcu-array jpeg-mcu-array))

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

(define-record-type <component>
  (make-component id index samp-x samp-y q-table)
  component?
  (id component-id)
  (index component-index)
  (samp-x component-samp-x)
  (samp-y component-samp-y)
  (q-table component-q-table))

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
           (table (make-huffman-table size-counts values)))
      (match Tc
        (0 (vector-set! dc-tables Th table))
        (1 (vector-set! ac-tables Th table))
        (_ (error "Bad Tc value" Tc))))))

(define *initial-params*
  (make-params (make-vector 4 #f)
               (make-vector 4 #f)
               (make-vector 4 #f)
               0
               '()))

(define* (read-params port previous-params with-misc-sections?)
  (let* ((q-tables (vector-copy (q-tables previous-params)))
         (dc-tables (vector-copy (dc-tables previous-params)))
         (ac-tables (vector-copy (ac-tables previous-params)))
         (restart-interval (restart-interval previous-params))
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

;; return current dc
(define (read-block bit-port block prev-dc-q q-table dc-table ac-table)
  (define (record! index quantized-coefficient)
    (let* ((index (bytevector-u8-ref normal-order index))
           (q (vector-ref q-table index)))
      (vector-set! block index (* quantized-coefficient q))))
  ;; First, read DC coefficient.
  (let* ((dc-diff-bits (read-huffman-coded-value bit-port dc-table))
         (dc-qdiff (read-signed-bits bit-port dc-diff-bits))
         (dc-q (+ prev-dc-q dc-qdiff)))
    (record! 0 dc-q)
    ;; Now read AC coefficients.
    (let lp ((k 1))
      (let* ((code (read-huffman-coded-value bit-port ac-table)))
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

(define* (read-jpeg port #:key (with-body? #t) (with-misc-sections? #t))
  (read-soi port)
  (call-with-values (lambda ()
                      (read-params port *initial-params* with-misc-sections?))
    (lambda (image-params sof)
      (let* ((frame (read-frame-header port sof))
             (dest (allocate-dct-matrix frame)))
        (let lp ((params image-params) (misc (misc-segments image-params)))
          (call-with-values (lambda ()
                              (read-params port params with-misc-sections?))
            (lambda (scan-params marker)
              (case marker
                ((#xffd9)             ; EOI
                 (make-jpeg frame misc dest))
                ((#xffda)             ; SOS
                 (cond
                  (with-body?
                   (read-scan port frame scan-params dest)
                   (lp scan-params (append misc (misc-segments scan-params))))
                  (else
                   (make-jpeg frame misc dest))))
                (else
                 (error "Unexpected marker" marker))))))))))

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

(define (compute-block-codes block q-table prev-dc)
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

(define (compute-code-sequences jpeg)
  (define (compute-scan-components frame mcu-array)
    (array-map-values
     (lambda (component)
       (let ((q-table (vector-ref q-tables (component-q-table component))))
         ;; We don't know the dc and ac huffman tables yet.
         (vector component 0 q-table #f #f)))
     (frame-components frame)))
  (match jpeg
    (($ <jpeg> frame misc mcu-array)
     (let ((scan-components (compute-scan-components frame mcu-array))
           (q-tables (q-tables-for-mcu-array mcu-array)))
       (values
        q-tables
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
                          (call-with-values
                              (lambda ()
                                (compute-block-codes block q-table dc))
                            (lambda (dc codes)
                              (values dc (cons codes out)))))
                        blocks prev-dc '()))
                   (lambda (dc out)
                     (vector-set! scan-component 1 dc)
                     (reverse out))))))
            mcu
            scan-components))
         mcu-array))))))

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
       (flush-bits port)))))

(define (write-eoi port)
  (put-u16 port #xffd9)) ; EOI.

(define (write-jpeg port jpeg)
  (match jpeg
    (($ <jpeg> frame misc mcu-array)
     (call-with-values (lambda () (compute-code-sequences jpeg))
       (lambda (q-tables codes)
         (let* ((frequencies (compute-code-frequencies codes))
                (huffman-tables (compute-huffman-code-tables frequencies)))
           (write-soi port)
           (for-each (lambda (misc) (write-misc-segment port misc)) misc)
           (write-baseline-frame port frame)
           (write-q-tables port q-tables)
           (write-huffman-tables port huffman-tables)
           (write-baseline-scan-header port frame)
           (write-baseline-entropy-coded-data port codes huffman-tables)
           (write-eoi port)))))))
