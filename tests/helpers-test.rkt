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

; hex-string?
(check-true (hex-string? "0A"))
(check-true (hex-string? "A"))
(check-true (hex-string? "a"))
(check-true (hex-string? "1"))
(check-false (hex-string? ""))
(check-false (hex-string? 0))

; tox-id?
(check-true (tox-id? "802D30E27746AE299FC2796D014C24700140574BFBFBB9397114D7CB82DC25728BA74CC378EF"))
(check-false (tox-id? "802D30E27746AE299FC2796D014C24700140574BFBFBB9397114D7CB82DC25728BA74CC378EFAAAAAAAAAA"))
(check-false (tox-id? "80"))
(check-exn exn:fail:contract?
           (λ ()
             (tox-id? 0)))

; delnode
(check-equal? (delnode '(1 2 3 4 5) 2) '(1 2 4 5))
(check-equal? (delnode '(1) 0) '())
(check-equal? (delnode '(1 2 3) 0) '(2 3))
(check-equal? (delnode '(1 2 3) 2) '(1 2))
(check-equal? (delnode '() 0) '())
(check-equal? (delnode '() 10) '()) ; Is this behavior wanted?
(check-exn exn:fail:contract?
           (λ ()
             (delnode '(1 2 3) 4)))
(check-exn exn:fail:contract?
           (λ ()
             (delnode '(1 2 3 4) -1)))
(check-exn exn:fail:contract?
           (λ ()
             (delnode '(1 2 3 4) 3/4)))

; linky?
; need to understand GUI better before implementing or testing this
