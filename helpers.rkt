#lang racket/gui
; helpers.rkt
; contains miscellaneous procedures for things
(provide (all-defined-out))
(require net/url)

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
        (cond [(string=? (substring str 0 8) "https://") #t]
              [else #f])
        (raise-argument-error 'http? "string?" str))))

; accepts a string and singles out the hyperlink
; "you're mom likes https://github.com/ and stuff"
; -> "https://github.com/"
(define grab-http
  (λ (str)
    ; incomplete grab after ? (index.php?somethinghere)
    ; incomplete grab if the URL has - or _
    (let ((url (regexp-match #px"https?://(\\w*\\.)*\\w*(/\\w*)*(\\?\\w*(#\\w*)?)?" str)))
      (if (false? url)
          null
          (first url)))))

; regex for tox://
;"^((?P<scheme>tox)://)?((?P<tox_id>[[:xdigit:]]{%i})|(?P<authority_user
;[[:digit:][:alpha:]]+)(@(?P<authority_host>[[:digit:][:alpha:]]+(\\.[[:digit:][:alpha:]]+)+))?)"

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

; takes a given string and makes it all blue 'n' shit
; uses racket/gui to make it blue (and underlined?)
; uses net/url to make it clickable?
(define linkify
  (λ (str)
    (let* ((http (grab-http str))
           (url (if (null? http)
                    null
                    (string->url http))))
      (if (null? url)
          null
          #|colorify the string
          clickify the link|#
          (printf "~a\n" url)))))
