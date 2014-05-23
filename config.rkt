#lang racket
; config.rkt
; contains default values for variables
(require json)
(provide (all-defined-out))

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
(define data-file (build-path tox-path "blight-data")) #|DONT' FORGET TO CHANGE BACK THIS ONCE STABLE|#
; blight-specific configurations
(define config-file (build-path tox-path "blight-config.json"))

; tox-path doesn't exist, create it
(unless (directory-exists? tox-path)
  (make-directory tox-path))

; if blight-config.json does not exist, create it
(unless (file-exists? config-file)
  (define config-port-out
    (open-output-file config-file
                      #:mode 'text
                      #:exists 'can-update))
  (printf "~a created...\n" config-file)
  (close-output-port config-port-out))

; open blight-config.json
(define config-port-in (open-input-file config-file
                                        #:mode 'text))

; if data does not exist, create it
(unless (file-exists? data-file)
  (define data-port-out
    (open-output-file data-file
                      #:mode 'binary
                      #:exists 'can-update))
  (printf "~a created...\n" data-file)
  (close-output-port data-port-out))

; default name and status message
; if data exists, do no use these
(define my-name-default "Blight Tester")
(define my-status-message-default "Toxing on Blight")

; default DHT settings
; if blight-config.json exists, do not use these
(define dht-address-default "192.254.75.98")
(define dht-port-default 33445)
(define dht-public-key-default
  "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074")

; if blight-config.json does not exist, initalize it to default values
(define json-default
  (hasheq 'dht-address dht-address-default
          'dht-port dht-port-default
          'dht-public-key dht-public-key-default
          'my-name-last my-name-default
          'my-status-last my-status-message-default))

; blight-config.json is empty, initialize with default values for variables
(unless (not (zero? (file-size config-file)))
  (let ((config-port-out (open-output-file config-file
                                           #:mode 'text
                                           #:exists 'truncate/replace)))
    (json-null 'null)
    (write-json json-default config-port-out)
    (write-json (json-null) config-port-out)
    (close-output-port config-port-out)))
