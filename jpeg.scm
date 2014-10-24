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
;; A parser for EXIF and JPEG.
;;
;;; Code:

(define-module (jpeg)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (jpeg exif)
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
