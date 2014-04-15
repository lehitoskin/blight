#lang racket
; callbacks.rkt
(provide (all-defined-out))

#| ###########START CALLBACK PROCEDURE DEFINITIONS########## |#
; inner procedure callback for tox_callback_connection_status
(define on-connection-change
  (λ (mtox pub-key data length userdata)
    (displayln "There's been a change in connection")))
