;; guile-jpeg
;; Copyright (C) 2014 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A parser for EXIF and JPEG.
;;
;;; Code:

(define-module (jpeg)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (parse-exif
            jpeg-dimensions
            jpeg-dimensions-and-exif))




;; Exif version 2.3:
;; http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf

(define *exif-tag-names* (make-hash-table))

(define-syntax-rule (define-exif-tags table (value name) ...)
  (begin
    (hashv-set! table value 'name)
    ...))

;; EXIF v2.3, table 4.
(define-exif-tags *exif-tag-names*
  ;; Image structure.
  (#x100 image-width)
  (#x101 image-length)
  (#x102 bits-per-sample)
  (#x103 compression)
  (#x106 photometric-interpretation)
  (#x112 orientation)
  (#x115 samples-per-pixel)
  (#x11c planar-configuration)
  (#x212 y-cb-cr-sub-sampling)
  (#x213 y-cb-cr-positioning)
  (#x11a x-resolution)
  (#x11b y-resolution)
  (#x128 resolution-unit)
  ;; Offsets.
  (#x111 strip-offsets)
  (#x116 rows-per-strip)
  (#x117 strip-byte-counts)
  (#x201 jpeg-interchange-format)
  (#x202 jpeg-interchange-format-length)
  ;; Image data characteristics.
  (#x12d transfer-function)
  (#x13e white-point)
  (#x13f primary-chromaticities)
  (#x211 y-cb-cr-coefficients)
  (#x214 reference-black-white)
  ;; Other tags.
  (#x132 date-time)
  (#x10e image-description)
  (#x10f make)
  (#x110 model)
  (#x131 software)
  (#x13b artist)
  (#x8298 copyright))

;; EXIF v2.3, table 7.
(define-exif-tags *exif-tag-names*
  ;; Version
  (#x9000 exif-version)
  (#xa000 flashpix-version)
  ;; Image data characteristics.
  (#xa001 color-space)
  (#xa500 gamma)
  ;; Image configuration.
  (#x9101 components-configuration)
  (#x9102 compressed-bits-per-pixel)
  (#xa002 pixel-x-dimension)
  (#xa003 pixel-y-dimension)
  ;; User information.
  (#x927c maker-note)
  (#x9286 user-comment)
  ;; Related files.
  (#xa004 related-sound-file)
  ;; Date and time.
  (#x9003 date-time-original)
  (#x9004 date-time-digitized)
  (#x9290 sub-sec-time)
  (#x9291 sub-sec-time-original)
  (#x9292 sub-sec-time-digitized)
  ;; Other.
  (#xa420 image-unique-id)
  (#xa430 camera-owner-name)
  (#xa431 body-serial-number)
  (#xa432 lens-specification)
  (#xa433 lens-make)
  (#xa434 lens-model)
  (#xa435 lens-serial-number))

;; EXIF v2.3, table 8.
(define-exif-tags *exif-tag-names*
  ;; Picture-taking conditions.
  (#x829a exposure-time)
  (#x829d f-number)
  (#x8822 exposure-program)
  (#x8824 spectral-sensitivity)
  (#x8827 photographic-sensitivity)
  (#x8828 oecf)
  (#x8830 sensitivity-type)
  (#x8831 standard-output-sensitivity)
  (#x8832 recommended-exposureindex)
  (#x8833 iso-speed)
  (#x8834 iso-speed-latitude-yyy)
  (#x8835 iso-speed-latitude-zzz)
  (#x9201 shutter-speed-value)
  (#x9202 aperture-value)
  (#x9203 brightness-value)
  (#x9204 exposure-bias-value)
  (#x9205 max-aperture-value)
  (#x9206 subject-distance)
  (#x9207 metering-mode)
  (#x9208 light-source)
  (#x9209 flash)
  (#x920a focal-length)
  (#x9214 subject-area)
  (#xa20b flash-energy)
  (#xa20c spatial-frequency-response)
  (#xa20e focal-plane-x-resolution)
  (#xa20f focal-plane-y-resolution)
  (#xa210 focal-plane-resolution-unit)
  (#xa214 subject-location)
  (#xa215 exposure-index)
  (#xa217 sensing-method)
  (#xa300 file-source)
  (#xa301 scene-type)
  (#xa302 cfa-pattern)
  (#xa401 custom-rendered)
  (#xa402 exposure-mode)
  (#xa403 white-balance)
  (#xa404 digital-zoom-ratio)
  (#xa405 focal-length-in-35mm-film)
  (#xa406 scene-capture-type)
  (#xa407 gain-control)
  (#xa408 contrast)
  (#xa409 saturation)
  (#xa40a sharpness)
  (#xa40b device-settings-description)
  (#xa40c subject-distance-range))

(define *type-widths*
  #(#f ; 0 is unused.
    1 1 2 4 8 ; BYTE ASCII SHORT LONG RATIONAL
    1 1 2 4 8 ; SBYTE UNDEFINED SSHORT SLONG SRATIONAL
    4 8 ; FLOAT DOUBLE
    ))

(define *type-parsers*
  (vector
   #f                                                  ; 0 is unused.
   (lambda (bv pos order) (bytevector-u8-ref bv pos))  ; BYTE
   (lambda (bv pos order) (error "unreachable"))       ; ASCII
   (lambda (bv pos order) (bytevector-u16-ref bv pos order))  ; SHORT
   (lambda (bv pos order) (bytevector-u32-ref bv pos order))  ; LONG
   (lambda (bv pos order)
     (cons (bytevector-u32-ref bv pos order)
           (bytevector-u32-ref bv (+ pos 4) order)))   ; RATIONAL
   (lambda (bv pos order) (bytevector-s8-ref bv pos))  ; SBYTE
   (lambda (bv pos order) (error "unreachable"))       ; UNDEFINED
   (lambda (bv pos order) (bytevector-s16-ref bv pos order))  ; SSHORT
   (lambda (bv pos order) (bytevector-s32-ref bv pos order))  ; SLONG
   (lambda (bv pos order)
     (cons (bytevector-u32-ref bv pos order)
           (bytevector-u32-ref bv (+ pos 4) order))) ; SRATIONAL
   (lambda (bv pos order) (bytevector-ieee-single-ref bv pos order)) ; FLOAT
   (lambda (bv pos order) (bytevector-ieee-double-ref bv pos order)) ; DOUBLE
   ))

(define (type-width type)
  (and (< type (vector-length *type-widths*))
       (vector-ref *type-widths* type)))

(define (type-parser type)
  (and (< type (vector-length *type-parsers*))
       (vector-ref *type-parsers* type)))

(define (read-value bv pos order type count)
  (case type
    ((2) ; ASCII
     (if (> count 0)
         ;; Trim trailing NUL byte.
         (let ((res (make-bytevector (1- count))))
           (bytevector-copy! bv pos res 0 (1- count))
           (utf8->string res))
         ""))
    ((7) ; UNDEFINED
     (let ((res (make-bytevector count)))
       (bytevector-copy! bv pos res 0 count)
       res))
    (else
     (let ((parser (type-parser type)))
       (and parser
            (if (= count 1)
                (parser bv pos order)
                (let ((res (make-vector count))
                      (width (type-width type)))
                  (let lp ((n 0) (pos pos))
                    (if (< n count)
                        (begin
                          (vector-set! res n (parser bv pos order))
                          (lp (1+ n) (+ pos width)))
                        res)))))))))

(define *value-interpreters* (make-hash-table))

(define-syntax-rule (define-value-interpreter (name value) body ...)
  (hashq-set! *value-interpreters* 'name
              (lambda (value) body ...)))

(define-value-interpreter (orientation value)
  (case value
    ((1) "Normal")
    ((2) "Mirrored")
    ((3) "Rotated 180 degrees")
    ((4) "Rotated 180 degrees then mirrored")
    ((5) "Rotated 90 degrees clockwise then mirrored")
    ((6) "Rotated 90 degrees clockwise")
    ((7) "Rotated 90 degrees counter-clockwise then mirrored")
    ((8) "Rotated 90 degrees counter-clockwise")))

(define-value-interpreter (photometric-interpretation value)
  (case value
    ((2) "RGB")
    ((6) "YCbCr")
    (else value)))

(define-value-interpreter (planar-configuratino value)
  (case value
    ((1) "Chunky")
    ((2) "Planar")
    (else value)))

(define-value-interpreter (y-cb-cr-positioning value)
  (case value
    ((1) "Centered")
    ((2) "Co-sited")
    (else value)))

(define-value-interpreter (resolution-unit value)
  (case value
    ((2) "Inches")
    ((3) "Centimeters")
    (else value)))

(define-value-interpreter (focal-plane-resolution-unit value)
  (case value
    ((2) "Inches")
    ((3) "Centimeters")
    (else value)))

(define-value-interpreter (compression value)
  (case value
    ((1) "Uncompressed")
    ((6) "JPEG")
    (else value)))

(define-value-interpreter (color-space value)
  (case value
    ((1) "sRGB")
    ((#xffff) "Uncalibrated")
    (else value)))

(define-value-interpreter (exposure-program value)
  (case value
    ((1) "Manual")
    ((2) "Normal")
    ((3) "Aperture priority")
    ((4) "Shutter priority")
    ((5) "Creative")
    ((6) "Action")
    ((7) "Portrait")
    ((8) "Landscape")
    (else value)))

(define-value-interpreter (sensitivity-type value)
  (case value
    ((1) "SOS")
    ((2) "REI")
    ((3) "ISO")
    ((4) "SOS+REI")
    ((5) "SOS+ISO")
    ((6) "REI+ISO")
    ((7) "SOS+REI+ISO")
    (else value)))

(define-value-interpreter (metering-mode value)
  (case value
    ((1) "Average")
    ((2) "Center-weighted average")
    ((3) "Spot")
    ((4) "Multi-spot")
    ((5) "Pattern")
    ((6) "Partial")
    (else value)))

(define-value-interpreter (light-source value)
  (case value
    ((1) "Daylight")
    ((2) "Flourescent")
    ((3) "Tungsten")
    ((4) "Flash")
    ((9) "Fine weather")
    ((10) "Cloudy weather")
    ((11) "Shade")
    ((12) "Daylight flourescent")
    ((13) "Day white flourescent")
    ((14) "Cool white flourescent")
    ((15) "White flourescent")
    ((16) "Warm white flourescent")
    ((17) "Standard light A")
    ((18) "Standard light B")
    ((19) "Standard light C")
    ((20) "D55")
    ((21) "D65")
    ((22) "D75")
    ((23) "D50")
    ((24) "ISO studio tungsten")
    (else value)))

(define-value-interpreter (flash value)
  (let ((fired? (logbit? 0 value))
        (return (logand #b11 (ash value -1)))
        (mode (logand #b11 (ash value -3)))
        (present? (logbit? 5 value))
        (red-eye? (logbit? 6 value)))
    `((fired? . ,fired?)
      (return-light . ,(case return
                         ((0) "Not available")
                         ((1) "Unknown")
                         ((2) "Not detected")
                         ((3) "Detected")))
      (mode . ,(case mode
                 ((0) "Unknown")
                 ((1) "Compulsory firing")
                 ((2) "Compulsury suppression")
                 ((3) "Auto")))
      (present? . ,present?)
      (red-eye? . ,red-eye?))))

(define-value-interpreter (sensing-method value)
  (case value
    ((1) "Not defined")
    ((2) "One chip color area")
    ((3) "Two chip color area")
    ((4) "Three chip color area")
    ((5) "Color sequental area")
    ((6) "Trilinear")
    ((7) "Color sequental linear")
    (else value)))

(define-value-interpreter (file-source value)
  (case value
    ((1) "Transparent scanner")
    ((2) "Reflex scanner")
    ((3) "DSC")
    (else value)))

(define-value-interpreter (custom-rendered value)
  (case value
    ((1) "Normal")
    ((2) "Custom")
    (else value)))

(define-value-interpreter (exposure-mode value)
  (case value
    ((0) "Auto")
    ((1) "Manual")
    ((2) "Auto bracket")
    (else value)))

(define-value-interpreter (white-balance value)
  (case value
    ((0) "Auto")
    ((1) "Manual")
    (else value)))

(define-value-interpreter (screen-capture-type value)
  (case value
    ((0) "Standard")
    ((1) "Landscape")
    ((2) "Portrait")
    ((3) "Night")
    (else value)))

(define-value-interpreter (gain-control value)
  (case value
    ((0) "None")
    ((1) "Low gain up")
    ((2) "High gain up")
    ((3) "Low gain down")
    ((4) "High gain down")
    (else value)))

(define-value-interpreter (contrast value)
  (case value
    ((0) "Normal")
    ((1) "Soft")
    ((2) "Hard")
    (else value)))

(define-value-interpreter (saturation value)
  (case value
    ((0) "Normal")
    ((1) "Low")
    ((2) "High")
    (else value)))

(define-value-interpreter (sharpness value)
  (case value
    ((0) "Normal")
    ((1) "Soft")
    ((2) "Hard")
    (else value)))

(define-value-interpreter (subject-distance-range value)
  (case value
    ((1) "Macro")
    ((2) "Close")
    ((3) "Distant")
    (else value)))

(define (interpret-value name value)
  (let ((interpret (hashq-ref *value-interpreters* name)))
    (if interpret
        (interpret value)
        value)))

(define (parse-ifd-chain bv pos order max-depth)
  (define (inline-value? type count)
    (let ((byte-width (and (< type (vector-length *type-widths*))
                           (vector-ref *type-widths* type))))
      (and byte-width (<= (* byte-width count) 4))))
  (define (parse-tags tag-count)
    (let lp ((n 0))
      (if (< n tag-count)
          (let* ((pos (+ pos 2 (* n 12)))
                 (tag (bytevector-u16-ref bv pos order))
                 (name (hashv-ref *exif-tag-names* tag tag))
                 (type (bytevector-u16-ref bv (+ pos 2) order))
                 (count (bytevector-u32-ref bv (+ pos 4) order))
                 (offset (if (inline-value? type count)
                             (+ pos 8)
                             (bytevector-u32-ref bv (+ pos 8) order)))
                 (value (read-value bv offset order type count)))
            (if (and (eqv? tag #x8769) ;; Nested EXIF information.
                     (integer? value)
                     (positive? max-depth))
                (match (parse-ifd-chain bv value order (1- max-depth))
                  ((alist)
                   (append alist (lp (1+ n)))))
                (let ((value* (interpret-value name value)))
                  (acons name value* (lp (1+ n))))))
          '())))
  (let* ((tag-count (bytevector-u16-ref bv pos order))
         (next-pos-offset (+ pos 2 (* tag-count 12)))
         (next-pos (bytevector-u32-ref bv next-pos-offset order)))
    (cons (parse-tags tag-count)
          (cond
           ((or (zero? next-pos) (not (positive? max-depth))) '())
           (else (parse-ifd-chain bv next-pos order (1- max-depth)))))))

(define (parse-exif bv)
  (define (parse-byte-order b0 b1)
    (unless (= b0 b1) (error "Bad TIFF header prefix"))
    (case (integer->char b0)
      ((#\I) (endianness little))
      ((#\M) (endianness big))
      (else (error "Bad TIFF header byte order"))))
  (let ((order (parse-byte-order (bytevector-u8-ref bv 0)
                                 (bytevector-u8-ref bv 1))))
    (unless (= 42 (bytevector-u16-ref bv 2 order))
      (error "Bad TIFF header magic value"))
    (let ((ifd0 (bytevector-u32-ref bv 4 order)))
      ;; Root IFD -> embedded EXIF -> one more
      (define *max-exif-depth* 2)
      (parse-ifd-chain bv ifd0 order *max-exif-depth*))))




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

(define (read-soi port)
  (assert-marker port #xffd8))

(define (discard-misc-sequence port)
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
         (discard-misc-sequence port)))
      (else marker))))

(define (extract-exif-from-misc-sequence port)
  (define (seek-and-continue offset)
    (seek port offset SEEK_CUR)
    (extract-exif-from-misc-sequence port))
  (let ((marker (read-marker port)))
    (case marker
      ((#xffe1) ; APP1
       (let* ((len (read-u16 port))
              (payload-len (- len 2)))
         (unless (>= payload-len 0)
           (error "Invalid marker segment length" marker len))
         (if (>= payload-len 6)
             (if (let ((header (get-bytevector-n port 6)))
                   ;; Does the APP1 section start with "Exif",
                   ;; zero-padded to 6 bytes?
                   (equal? header #vu8(69 120 105 102 0 0)))
                 (let ((content (get-bytevector-n port (- payload-len 6))))
                   (cons (parse-exif content)
                         (extract-exif-from-misc-sequence port)))
                 (seek-and-continue (- payload-len 6)))
             (seek-and-continue payload-len))))
      ((#xffdb ; DQT
        #xffc4 ; DHT
        #xffcc ; DAC
        #xffdd ; DRI
        #xfffe ; COM
        #xffe0 #xffe2 #xffe3 #xffe4 #xffe5 #xffe6 #xffe7 ; APP0, APP2-APP7
        #xffe8 #xffe9 #xffea #xffeb #xffec #xffed #xffee #xffef) ; APP8-APP15
       (let* ((len (read-u16 port))
              (payload-len (- len 2)))
         (unless (>= payload-len 0)
           (error "Invalid marker segment length" marker len))
         (seek-and-continue payload-len)))
      (else (list marker)))))

(define-record-type <frame-header>
  (make-frame-header marker precision y x component-count components)
  frame-header?
  (marker frame-header-marker)
  (precision frame-header-precision)
  (y frame-header-y)
  (x frame-header-x)
  (component-count frame-header-component-count)
  (components frame-header-components))

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
              (component-count (read-u8 port))
              (skip (- len (+ 8 (* component-count 3)))))
         (unless (>= skip 0)
           (error "Invalid frame header segment length" sof len))
         (let ((components
                (let lp ((n 0))
                  (if (= n component-count)
                      '()
                      (let ((id (read-u8 port))
                            (samp (read-u8 port))
                            (table (read-u8 port)))
                        (cons (vector id (ash samp -4) (logand samp #xf) table)
                              (lp (1+ n))))))))
           (unless (zero? skip) (seek port skip SEEK_CUR))
           (make-frame-header sof precision y x component-count components)))))
    (else (error "Invalid start-of-frame marker" sof))))

(define (discard-scan port)
  (assert-marker port #xffda) ; SOS
  (let* ((len (read-u16 port))
         (payload-len (- len 2)))
    (unless (>= payload-len 0)
      (error "Invalid scan segment length" len))
    (seek port payload-len SEEK_CUR)))

(define (read-dnl port)
  (assert-marker port #xffdc) ; DNL
  (unless (eqv? (read-u16 port) 4)
    (error "Malformed DNL segment length"))
  (read-u16 port))

(define (jpeg-dimensions file)
  (let ((port (open-input-file file)))
    (read-soi port)
    (let* ((sof (discard-misc-sequence port))
           (header (read-frame-header port sof))
           (x (frame-header-x header))
           (y (frame-header-y header)))
      (if (zero? y)
          (begin
            (discard-scan port)
            (values x (read-dnl port)))
          (values x y)))))

(define (jpeg-dimensions-and-exif file)
  (let ((port (open-input-file file)))
    (read-soi port)
    (match (extract-exif-from-misc-sequence port)
      ((exif ... sof)
       (let* ((header (read-frame-header port sof))
              (x (frame-header-x header))
              (y (frame-header-y header))
              (exif (match exif
                      (((main thumbnail)) main)
                      (((main)) main)
                      (_ '()))))
         (if (zero? y)
             (begin
               (discard-scan port)
               (values x (read-dnl port) exif))
             (values x y exif)))))))
