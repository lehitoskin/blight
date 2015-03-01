#lang typed/racket/base
; number-conversions.rkt
; provides several conversions from and to
; decimal and hexadecimal
(provide dec->hex
         hex->dec)

(define (whole-number? (val : Any)) : Boolean
  (and (integer? val)
       (or (zero? val) (positive? val))))

; takes a number, returns a string
(define (dec->hex (x : Nonnegative-Integer)) : String
  (if (< x 16)
      ; Leading zero so it can be used with Tox IDs.
      (string-append "0" (string-upcase (number->string x 16)))
      (number->string x 16)))

; takes a string, returns a number
(define (hex->dec (x : String)) : (U Complex False)
  (string->number x 16))
