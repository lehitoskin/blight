#lang typed/racket/base

(require typed/rackunit
         "../helpers.rkt")

; any->bool
(assert (any->bool 'a) boolean?)
(assert (any->bool #f) boolean?)
(check-true (any->bool 'a))
(check-false (any->bool #f))

; hex-string?
(assert (hex-string? "DEADBEEF") boolean?)
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
#;(check-exn exn:fail:contract?
           (λ ()
             (tox-id? 0)))

; (get-time)
(assert (get-time) string?)
