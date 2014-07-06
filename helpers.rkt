#lang racket
; helpers.rkt
; contains miscellaneous procedures for things
(provide http?
         https?
         grab-http
         any->bool
         hex-string?
         tox-id?
         delnode
         setnode
         get-time)

(require #;net/url
         racket/date)

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
        #f
        (first url))))

; regex for tox://
;^((?P<scheme>tox)://)?((?P<tox_id>[[:xdigit:]]{%i})|(?P<authority_user
;[[:digit:][:alpha:]]+)(@(?P<authority_host>[[:digit:][:alpha:]]+(\\.[[:digit:][:alpha:]]+)+))?)

(define/contract any->bool
  (-> any/c boolean?)
  (λ (x)
    (not (not x))))

; checks if a given string is in hexadecimal format
(define/contract hex-string?
  (-> any/c boolean?)
  (λ (val)
    (and (string? val)
         ; number-conversions can't convert "" hex-strings so they aren't valid.
         (not (zero? (string-length val)))
         (any->bool (andmap (λ (a-char)
                              (or (char-numeric? a-char)
                                  (member a-char (list #\a #\b #\c #\d #\e #\f))))
                            (string->list (string-downcase val)))))))

; checks if a given string is a valid 76-character Tox ID
(define/contract tox-id?
  (-> string? boolean?)
  (λ (str)
    (and (= (string-length str) 76)
         (hex-string? str))))

; procedure to remove a specific node in a list
(define/contract delnode
  (-> list? (and/c integer? (negate negative?)) list?)
  (λ (lst num)
    (when (>= num (length lst))
      (raise-range-error 'delnode "list" "given " num lst 0 (sub1 (length lst))))
    (append (take lst num)
            (drop lst (add1 num)))))

; procedure to replace a specific node in a list
(define/contract setnode
  (-> list? any/c (and/c integer? (negate negative?)) list?)
  (λ (lst node num)
    (when (>= num (length lst))
      (raise-range-error 'setnode "list" "given " num lst 0 (sub1 (length lst))))
    ;; This will be a bit of a hack due to poor stdlib support. There is a
    ;; third party library that can handle this, but lethoskin has to be
    ;; convinced to use it first.
    ;; http://planet.racket-lang.org/display.ss?package=ralist.plt&owner=dvanhorn
    ;;
    ;; There's also a list-set that can be found in the unsafe section of
    ;; the stdlib, but that part of the library is unsafe.
    ;;
    ;; Anyway, lists don't have list-set because vectors are supposed to be
    ;; used for random access lookup, but vectors don't have vector-set
    ;; because they aren't supported very well. Lists are also immutable so
    ;; they have to be converted into a vector before they can be mutated.
    ;;
    ;; Forgive me lisp alien, for I have sinned!
    (define a-vector (list->vector lst))
    (vector-set! a-vector num node)
    (vector->list a-vector)))
    
                 

; get the current time formatted to HH:MM:SS
(define get-time
  (λ ()
    (let* ((timestamp (current-date))
           (ts-hour (date-hour timestamp))
           (ts-minute (date-minute timestamp))
           (ts-second (date-second timestamp)))
      (define hour (if (< ts-hour 10)
                       (string-append "0" (number->string ts-hour))
                       (number->string ts-hour)))
      (define minute (if (< ts-minute 10)
                         (string-append "0" (number->string ts-minute))
                         (number->string ts-minute)))
      (define second (if (< ts-second 10)
                         (string-append "0" (number->string ts-second))
                         (number->string ts-second)))
      (string-append hour ":" minute ":" second))))

; takes a given string and makes it all blue 'n' shit
; uses racket/gui to make it blue (and underlined?)
; uses net/url to make it clickable?
#|(define linkify
  (λ (str)
    (let* ((http (grab-http str))
           (url (if (false? http)
                    #f
                    (string->url http))))
      (if (flase? url)
          #f
          #|colorify the string
          clickify the link|#
          (printf "~a\n" url)))))|#
