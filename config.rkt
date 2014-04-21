#lang racket
; config.rkt
(provide (all-defined-out))

; check what system we're running on and look
; for the db in the appropriate location
(define db-path (cond [(eq? (system-type) 'unix)
                       (build-path (find-system-path 'home-dir)
                                   ".config/tox/blight-tox.db")]
                      ; /AppData/Roaming/tox/blight-tox.db
                      [(eq? (system-type) 'windows)
                       (normal-case-path
                        (build-path (find-system-path 'home-dir)
                                    "appdata/local/tox/blight-tox.db"))]
                      ; we're not running on windows or unix, use temp-dir
                      [else (string-append (path->string (find-system-path 'temp-dir))
                                           "/blight-tox.db")]))

; default name and status message
(define my-name "Blight Tester")
(define my-status-message "Toxing on Blight")

; default DHT settings
(define dht-address "192.254.75.98")
(define dht-port 33445)
(define dht-public-key "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074")
