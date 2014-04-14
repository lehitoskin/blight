#lang racket
; config.rkt
(provide (all-defined-out))
(require file/sha1)

; default name and status message
(define my-name "Blight Tester")
(define my-status-message "Toxing on Blight")

; default DHT settings
(define dht-address "192.254.75.98")
(define dht-port 33445)
(define dht-public-key
  (hex-string->bytes
   "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074"))
