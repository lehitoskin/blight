#lang racket
; number-conversions.rkt
; provides several conversions from and to
; binary, decimal, and hexadecimal

(require "helpers.rkt")

(provide dec->bin
         dec->hex
         
         bin->dec
         bin->hex
         
         hex->dec
         hex->bin)

(define (binary? val)
  (andmap (curryr member (list #\0 #\1))
          (string->list (number->string val))))

(define (whole-number? val)
  (and (integer? val)
       (or (zero? val) (positive? val))))

; takes a number, returns a number
(define/contract dec->bin
  (-> whole-number? binary?)
  (λ (x)
    (string->number (number->string x 2))))

; takes a number, returns a string
(define/contract dec->hex
  (-> whole-number? hex-string?)
  (λ (x)
    (if (< x 16)
        (string-append "0" (string-upcase (number->string x 16))) ; Leading zero so it can be used with Tox IDs.
        (number->string x 16))))

; takes a number, returns a number
(define/contract bin->dec
  (-> binary? whole-number?)
  (λ (x)
    (string->number (number->string x) 2)))

; takes a number, returns a string
(define/contract bin->hex
  (-> binary? hex-string?)
  (λ (x)
    (dec->hex (bin->dec x))))

; takes a string, returns a number
(define/contract hex->dec
  (-> hex-string? whole-number?)
  (λ (x)
    (string->number x 16)))

; takes a string, returns a number
(define/contract hex->bin
  (-> hex-string? binary?)
  (λ (x)
    (dec->bin (hex->dec x))))
