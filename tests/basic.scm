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
;; Test for (jpeg).
;;
;;; Code:

(define-module (tests jpeg)
  #:use-module (jpeg))

(define expected-width 500)
(define expected-height 375)
(define expected-exif
  '((make . "CAMERA                   ")
    (model . "DC2302                 ")
    (x-resolution 72 . 1)
    (y-resolution 72 . 1)
    (resolution-unit . "Inches")
    (software . "f-spot version 0.1.11")
    (date-time . "2006:05:14 20:55:54")
    (y-cb-cr-positioning . "Co-sited")
    (exposure-time 1 . 198)
    (f-number 971 . 100)
    (photographic-sensitivity . 50)
    (exif-version . #vu8(48 50 49 48))
    (date-time-original . "2004:10:31 03:03:17")
    (date-time-digitized . "2004:10:30 12:03:17")
    (components-configuration . #vu8(1 2 3 0))
    (shutter-speed-value 77 . 10)
    (aperture-value 5 . 1)
    (flash (fired? . #f)
           (return-light . "Not available")
           (mode . "Unknown")
           (present? . #f)
           (red-eye? . #f))
    (user-comment . #vu8(65 83 67 73 73 0 0 0))
    (flashpix-version . #vu8(48 49 48 48))
    (color-space . "sRGB")
    (pixel-x-dimension . 1600)
    (pixel-y-dimension . 1200)
    ;; This is an interoperability offset but because it is before the
    ;; EXIF segment we don't visit it; otherwise we would be exposed to
    ;; loop-like attacks.
    (40965 . 508)))

(call-with-values (lambda ()
                    (jpeg-dimensions-and-exif
                     (string-append (getenv "GUILE_JPEG_TOP_SRCDIR")
                                    "/tests/imag0029.normal.jpg")))
  (lambda (width height exif)
    (unless (equal? width expected-width)
      (error "not equal" width expected-width))
    (unless (equal? height expected-height)
      (error "not equal" height expected-height))
    (unless (equal? exif expected-exif)
      (error "not equal" exif expected-exif))))
