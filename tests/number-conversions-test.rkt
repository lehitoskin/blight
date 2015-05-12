#lang typed/racket/base

(require typed/rackunit
         "../number-conversions.rkt")

;; dec->hex
(check-equal? (dec->hex 0) "00")
(check-equal? (dec->hex 2) "02")
(check-equal? (dec->hex 15) "0F")
(check-equal? (dec->hex 16) "10")
(check-equal? (dec->hex 17) "11")
(check-equal? (dec->hex 32) "20")
(check-equal? (dec->hex 3735928559) "deadbeef")
#;(check-exn exn:fail:contract?
           (λ ()
             (dec->hex 0.5)))
#;(check-exn exn:fail:contract?
           (λ ()
             (dec->hex -1)))

;; hex->dec
(check-equal? (hex->dec "0") 0)
(check-equal? (hex->dec "00") 0)
(check-equal? (hex->dec "10") 16)
(check-equal? (hex->dec "a") 10)
(check-equal? (hex->dec "A") 10)
(check-equal? (hex->dec "Aa") (+ 160 10))
(check-equal? (hex->dec "aA") (+ 160 10))
(check-equal? (hex->dec "deadbeef") 3735928559)
#;(check-exn exn:fail:contract?
           (λ ()
             (hex->dec 0)))
#;(check-exn exn:fail:contract?
           (λ ()
             (hex->dec "")))
#;(check-exn exn:fail:contract?
           (λ ()
             (hex->dec "xyz")))
