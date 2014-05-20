#lang racket
; callbacks.rkt
; using ffi/unsafe messes with define/contract, so instead of
; asking for my-tox directly, we pass a procedure that returns
; my-tox as a workaround
(provide (all-defined-out))

#| ###########START CALLBACK PROCEDURE DEFINITIONS########## |#
(define/contract on-friend-request
  (-> procedure? string? string? integer? void?)
  (λ (grab-tox public-key message message-length)
    (displayln "We got ourselves a friend request!")))

(define on-friend-message
  (λ ()
    (displayln "We got a message!")))

(define on-friend-actions
  (λ ()
    (displayln "We got an action!")))

(define/contract on-friend-name-change
  (-> procedure? integer? string? integer? void?)
  (λ (grab-tox friendnumber newname length)
    (let ((my-tox (grab-tox)))
      (displayln "Friend's name changed"))))

(define on-status-message-change
  (λ ()
    (displayln "Status message changed")))

(define on-status-type-change
  (λ ()
    (displayln "Status type changed")))

(define on-typing-change
  (λ ()
    (displayln "Typing change detected")))

(define on-read-receipt
  (λ ()
    (displayln "Message read receipt")))

(define on-connection-status-change
  (λ ()
    (displayln "There's been a change in connection")))
