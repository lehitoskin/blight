#lang racket/gui
; callbacks.rkt
; using ffi/unsafe messes with define/contract, so instead of
; asking for my-tox directly, we pass a procedure that returns
; my-tox as a workaround
(provide (all-defined-out))

#|(define friend-request-dialog (new dialog%
                                   [label "Friend request"]
                                   [style (list 'close-button)]))|#

#| ###########START CALLBACK PROCEDURE DEFINITIONS########## |#
#|(define/contract on-friend-request
  (-> void?)
  (λ ()
    (let ((mbox (message-box "Friend Request"
                             (string-append 
                              'uid " would like to add you as a friend!")
                             friend-request-dialog
                             (list 'ok-cancel))))
      (if (eq? mbox 'ok)
          'derp
          'herp))))|#
          

#|(define/contract on-friend-message
  (-> void?)
  (λ ()
    (displayln "We got a message!")))|#

(define/contract on-friend-actions
  (-> void?)
  (λ ()
    (displayln "We got an action!")))

(define/contract on-friend-name-change
  (-> void?)
  (λ ()
    (displayln "Friend's name changed")))

(define/contract on-status-message-change
  (-> void?)
  (λ ()
    (displayln "Status message changed")))

#|(define/contract on-status-type-change
  (-> void?)
  (λ ()
    (displayln "Status type change detected!")))|#

(define/contract on-typing-change
  (-> void?)
  (λ ()
    (displayln "Typing change detected")))

(define/contract on-read-receipt
  (-> void?)
  (λ ()
    (displayln "Message read receipt")))

(define/contract on-connection-status-change
  (-> void?)
  (λ ()
    (displayln "There's been a change in connection")))
