#lang racket

(require rackunit
         "../number-conversions.rkt")

;; dec->bin
(check-equal? (dec->bin 0) 0)
(check-equal? (dec->bin 2) 10)
(check-equal? (dec->bin 5) 101)
(check-exn exn:fail:contract?
           (λ ()
             (dec->bin 0.5)))
(check-exn exn:fail:contract?
           (λ ()
             (dec->bin -1)))

;; dec->hex
(check-equal? (dec->hex 0) "0")
(check-equal? (dec->hex 2) "2")
(check-equal? (dec->hex 15) "f")
(check-equal? (dec->hex 16) "10")
(check-equal? (dec->hex 17) "11")
(check-equal? (dec->hex 32) "20")
(check-exn exn:fail:contract?
           (λ ()
             (dec->hex 0.5)))
(check-exn exn:fail:contract?
           (λ ()
             (dec->hex -1)))

;; bin->dec
(check-equal? (bin->dec 0) 0)
(check-equal? (bin->dec 10) 2)
(check-equal? (bin->dec 101) 5)
(check-exn exn:fail:contract?
           (λ ()
             (bin->dec 2))) ; Don't worry Bender, two's don't exist!
(check-exn exn:fail:contract?
           (λ ()
             (bin->dec 0.5)))
(check-exn exn:fail:contract?
           (λ ()
             (bin->dec -1)))

;; bin->hex
(check-equal? (bin->hex 0) "0")
(check-equal? (bin->hex 10) "2")
(check-equal? (bin->hex 10000) "10")
(check-equal? (bin->hex 10001) "11")
(check-equal? (bin->hex 100000) "20")
(check-exn exn:fail:contract?
           (λ ()
             (bin->hex 2)))
(check-exn exn:fail:contract?
           (λ ()
             (bin->hex 0.5)))
(check-exn exn:fail:contract?
           (λ ()
             (bin->hex -1)))

;; hex->dec
(check-equal? (hex->dec "0") 0)
(check-equal? (hex->dec "10") 16)
(check-equal? (hex->dec "a") 10)
(check-equal? (hex->dec "A") 10)
(check-equal? (hex->dec "Aa") (+ 160 10))
(check-equal? (hex->dec "aA") (+ 160 10))
(check-exn exn:fail:contract?
           (λ ()
             (hex->dec 0)))
(check-exn exn:fail:contract?
           (λ ()
             (hex->dec "")))
(check-exn exn:fail:contract?
           (λ ()
             (hex->dec "xyz")))

;; hex->bin
(check-equal? (hex->bin "0") 0)
(check-equal? (hex->bin "10") 10000)
(check-equal? (hex->bin "11") 10001)
(check-equal? (hex->bin "a") 1010)
(check-equal? (hex->bin "A") 1010)
(check-equal? (hex->bin "aA") (dec->bin (+ 160 10)))
(check-equal? (hex->bin "Aa") (dec->bin (+ 160 10)))
(check-exn exn:fail:contract?
           (λ ()
             (hex->bin 0)))
(check-exn exn:fail:contract?
           (λ ()
             (hex->bin "")))
(check-exn exn:fail:contract?
           (λ ()
             (hex->bin "xyz")))