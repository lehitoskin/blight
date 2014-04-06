#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client
(require libtoxcore-racket ; wrapper
         db                 ; access db for stored info
         file/sha1)         ; hex-string procedures

; gui stuff
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Clock"]
                   [width 300]
                   [height 600]))

; make a static text message in the frame
(define msg (new message% [parent frame]
		          (label [number->string(current-seconds)])))

; make button in the frame
(new button% [parent frame]
             [label "Quit"]
	     ; callback procedure for a button click:
	     [callback (λ (button event)
			 (exit))])

; show the frame by call its show method
(send frame show #t)

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