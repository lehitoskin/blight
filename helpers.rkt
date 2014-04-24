#lang racket
; helpers.rkt
; contains miscellaneous procedures for things
(provide (all-defined-out))

(define http?
  (λ (str)
    (if (string? str)
        (cond [(string=? (substring str 0 7) "http://") #t]
              [(string=? (substring str 0 4) "www.") #t]
              [else #f])
        (raise-argument-error 'http? "string?" str))))

(define https?
  (λ (str)
    (if (string? str)
        (cond [(string=? (substring str 0 7) "https://") #t]
              [else #f])
        (raise-argument-error 'http? "string?" str))))

; accepts a string and singles out the hyperlink
(define grab-http 'str)

; checks if a given string is a valid 76-character Tox ID
; TODO: more checks to see if the characters in the string
; are valid for an ID
(define tox-id?
  (λ (str)
    (if (string? str)
        (cond [(= (string-length str) 76) #t]
              [else #f])
        (raise-argument-error 'tox-id? "string?" str))))
