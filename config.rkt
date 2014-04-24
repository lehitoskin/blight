#lang racket
; config.rkt
(provide (all-defined-out))
; check what system we're running on and look
; for the db in the appropriate location

; base tox directory
(define tox-path (cond [(eq? (system-type) 'unix)
                        (build-path (find-system-path 'home-dir)
                                    ".config/tox")]
                       [(eq? (system-type) 'windows)
                        (normal-case-path
                         (build-path (find-system-path 'home-dir)
                                     "appdata/local/tox"))]))

; history db file
(define db-file (build-path tox-path "blight-tox.db"))
; tox-specific information
(define data-file (build-path tox-path "data"))
; blight-specific configurations
(define config-file (build-path tox-path "blight-config.json"))

; default name and status message
(define my-name "Blight Tester")
(define my-status-message "Toxing on Blight")

; default DHT settings
; if blight-config.json exists, do not use these
(define dht-address "192.254.75.98")
(define dht-port 33445)
(define dht-public-key "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074")
