#lang typed/racket/base
; helpers.rkt
; contains miscellaneous procedures for things
(require typed/racket/date)

(provide any->bool
         hex-string?
         tox-id?
         get-time)

(define (any->bool (x : Any)) : Boolean
  (not (not x)))

(define-type Hexadecimal-String (Listof Char))

; checks if a given string is in hexadecimal format
(define (hex-string? (val : Any)) : Boolean
  (and (string? val)
       ; number-conversions can't convert "" hex-strings so they aren't valid.
       (not (zero? (string-length val)))
       (any->bool (andmap (λ ([a-char : Char])
                            (or (char-numeric? a-char)
                                (member a-char (list #\a #\b #\c #\d #\e #\f))))
                          (string->list (string-downcase val))))))

; checks if a given string is a valid 76-character Tox ID
(define (tox-id? (str : Any)) : Boolean
  (and (string? str)
       (hex-string? str)
       (= (string-length str) 76)))

; get the current time formatted to HH:MM:SS
(define (get-time) : String
  (let* ([timestamp : date (current-date)]
         [ts-hour : Nonnegative-Integer (date-hour timestamp)]
         [ts-minute : Nonnegative-Integer (date-minute timestamp)]
         [ts-second : Nonnegative-Integer (date-second timestamp)])
    (define hour : String (if (< ts-hour 10)
                              (string-append "0" (number->string ts-hour))
                              (number->string ts-hour)))
    (define minute : String (if (< ts-minute 10)
                                (string-append "0" (number->string ts-minute))
                                (number->string ts-minute)))
    (define second : String (if (< ts-second 10)
                                (string-append "0" (number->string ts-second))
                                (number->string ts-second)))
    (string-append hour ":" minute ":" second)))
