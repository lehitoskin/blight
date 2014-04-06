#!/usr/bin/env racket
#lang racket
; blight
; charterm-based Tox client - for now
; GUI is better, but I'm the derps with GUI
(require libtoxcore-racket ; wrapper
         db                 ; access db for stored info
         file/sha1          ; hex-string procedures
         (planet neil/charterm)) ; character-cell terminal interface

(let loop ()
  (with-charterm
   ;(charterm-clear-screen)
   (charterm-cursor 10 5)
   (charterm-display "Hello, ")
   (charterm-bold)
   (charterm-display "you")
   (charterm-normal)
   (charterm-display ".")
   (charterm-cursor 1 1)
   (charterm-display "Press a key...")
   (let ((key (charterm-read-key)))
     (charterm-cursor 1 1)
     (charterm-clear-line)
     (printf "You pressed: ~S\r\n" key)
     (if (eq? key #\Q)
         (displayln "Goodbye!")
         (loop)))))

; tox stuff
#|(define my-name "Blight Wizard")
(define my-status-message "Toxing on Blight")
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))

(tox_set_status_message my-tox my-status-message (string-length
                                                  my-status-message))

; connect to DHT
(define dht-address "192.254.75.98")
(define dht-port 33445)
(define dht-public-key
  (hex-string->bytes
   "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074"))

(tox_bootstrap_from_address my-tox dht-address TOX_ENABLE_IPV6_DEFAULT dht-port
                            dht-public-key)

(tox_kill my-tox)|#