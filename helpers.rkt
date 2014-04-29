#lang racket
; helpers.rkt
; contains miscellaneous procedures for things
(provide (all-defined-out))
(require racket/trace)

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

; checks if a given string is in hexadecimal format
(define hex?
  (λ (str length)
    (let ((hex-chars (string->list "0123456789ABCDEF"))
          (str (string-upcase str)))
      (if (zero? length)
          #t
          (cond [(list? (member (string-ref str (- length 1)) hex-chars))
                 (hex? (substring str 0 (- length 1)) (- length 1))]
                [else #f])))))

; checks if a given string is a valid 76-character Tox ID
; TODO: more checks to see if the characters in the string
; are valid for an ID
(define tox-id?
  (λ (str)
    (if (string? str)
        (cond [(and (= (string-length str) 76) (hex? str (string-length str))) #t]
              [else #f])
        (raise-argument-error 'tox-id? "string?" str))))
