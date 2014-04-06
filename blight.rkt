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
                   [label "Blight"]
                   [width 400]
                   [height 600]))

; make a static text message in the frame
(define msg (new message% [parent frame]
                 [label "Blight"]))

(define editor-canvas (new editor-canvas%
                           [parent frame]
                           [style (list 'control-border 'no-hscroll
                                        'auto-vscroll)]
                           [line-count 5]
                           [min-height 400]
                           [vert-margin 10]
                           [enabled #t]))

(define enter (new key-event%
                   [key-code #\return]))

(define text (new text%
                  [line-spacing 1.0]))

; derive a new canvas (a drawing window) class to handle events
(define tfield (new text-field%
                    [label "Message:"]
                    [parent frame]
                    [vert-margin 50]
                    [enabled #t]
                    [callback (λ (on-char enter)
                                (send text get-text))]))


(define panel (new horizontal-pane%
                   [parent frame]
                   [border 2]))

#|(new button% [parent panel]
     [label "Send"]
     [callback (λ (button event)
                 (send msg set-label "Butts!"))])|#

; make button in the frame
(new button% [parent panel]
     [label "Quit"]
     ; callback procedure for a button click:
     [callback (λ (button event)
                 (exit))])


; show the frame by call its show method
(send frame show #t)

; tox stuff
#|(define my-name "Blight Tester")
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