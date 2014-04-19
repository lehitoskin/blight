#lang racket
; number-conversions.rkt
; provides several conversions from and to
; binary, decimal, and hexadecimal
(provide (all-defined-out))

; takes a number, returns a number
(define dec->bin
  (λ (x)
    (if (not (integer? x))
        (raise-argument-error 'dec->bin "integer?" x)
        (string->number (number->string x 2)))))

; takes a number, returns a string
(define dec->hex
  (λ (x)
    (if (not (integer? x))
        (raise-argument-error 'dec->hex "integer?" x)
        (number->string x 16))))

; takes a number, returns a number
(define bin->dec
  (λ (x)
    (if (not (integer? x))
        (raise-argument-error 'bin->dec "integer?" x)
        (string->number (number->string x) 2))))

; takes a number, returns a string
(define bin->hex
  (λ (x)
    (if (integer? x)
        (dec->hex (bin->dec x))
        (raise-argument-error 'bin->hex "integer?" x))))

; takes a string, returns a number
(define hex->dec
  (λ (x)
    (if (string? x)
         (string->number x 16)
         (raise-argument-error 'hex->dec "string?" x))))

; takes a string, returns a number
(define hex->bin
  (λ (x)
    (if (string? x)
        (dec->bin (hex->dec x))
        (raise-argument-error 'hex->bin "string?" x))))
