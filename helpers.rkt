#lang racket/gui
; helpers.rkt
; contains miscellaneous procedures for things
(provide (all-defined-out))
(require net/url)

(define/contract http?
  (-> string? boolean?)
  (λ (str)
    (or (string=? (substring str 0 7) "http://")
        (string=? (substring str 0 4) "www."))))

(define/contract https?
  (-> string? boolean?)
  (λ (str)
    (string=? (substring str 0 8) "https://")))

; accepts a string and singles out the hyperlink
; "you're mom likes https://github.com/ and stuff"
; -> "https://github.com/"
(define/contract grab-http
  (-> string? (or/c null? string?))
  (λ (str)
    ; incomplete grab after ? (index.php?somethinghere)
    ; incomplete grab if the URL has - or _
    (define url (regexp-match #px"https?://(\\w*\\.)*\\w*(/\\w*)*(\\?\\w*(#\\w*)?)?" str))
    (if (false? url)
        null ; Can this return #f instead?
        (first url))))

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
(define/contract tox-id?
  (-> string? boolean?)
  (λ (str)
    (and (= (string-length str) 76)
         (hex? str (string-length str)))))

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
