#lang racket

(require rackunit
         "../helpers.rkt")

(define www-url "www.google.com")
(define http-url "http://github.com/lehitoskin/blight")
(define https-url "https://github.com/lehitoskin/blight")
(define ftp-url "ftp://ftp.funet.fi/")

; http?
(check-true (http? http-url))
(check-true (http? www-url))
(check-false (http? https-url))
(check-false (http? ftp-url))
(check-exn exn:fail:contract?
           (λ ()
             (http? 0)))

; https?
(check-false (https? http-url))
(check-false (https? www-url))
(check-true (https? https-url))
(check-false (https? ftp-url))
(check-exn exn:fail:contract?
           (λ ()
             (https? 0)))


; grab-http
(check-equal? (grab-http "My favorite part about using https://www.wikipedia.org/ is the part where I use it.")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "https://www.wikipedia.org/ https://www.not-wikipedia.org/")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "Yo, check out\n this seet\r\n awesome\t\t\t\n\v\r link https://www.wikipedia.org/")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "λλλhttps://www.wikipedia.org/λλλ")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "https://www.wikipedia.org/ <- check it")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "check it -> https://www.wikipedia.org/")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "\nhttps://www.wikipedia.org/")
              "https://www.wikipedia.org/")
(check-equal? (grab-http "https://www.wikipedia.org/\n")
              "https://www.wikipedia.org/")
(check-exn exn:fail:contract?
           (λ ()
             (grab-http 0)))
