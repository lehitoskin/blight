#lang racket/base
; audio.rkt
; contains stuff to manipulate audio 'n' stuff
(require libopenal-racket
         libtoxcore-racket/av
         "tox.rkt")

(provide (all-defined-out)
         close-device!
         destroy-context!
         delete-sources!
         gen-sources
         set-current-context
         stop-source)

(define cur-calls (make-hash))

(define device (open-device #f))
(define context (create-context device))
(void (set-current-context context))

; csettings is another struct, so we can get even more information from there
(struct friend-call (call-index friend-id csettings type alsource)
  #:transparent #:mutable)

(define do-add-call
  (λ (call-index friend-id csettings type)
    (let ([fd (friend-call call-index friend-id csettings type (car (gen-sources 1)))])
      (hash-set! cur-calls call-index fd))))

(define add-new-call
  (λ (mav call-index)
    (let* ([friend-id (get-peer-id my-av call-index 0)]
           [csettings (get-peer-csettings my-av call-index friend-id)]
           [type (ToxAvCSettings-call-type csettings)])
      (do-add-call call-index friend-id csettings type))))

(define friend-call-id
  (λ (call-index)
    (friend-call-call-index (hash-ref cur-calls call-index))))

#;(define friend-call-name
  (λ (mtox call-index)
    (let ([id (friend-call-id call-index)])
      (get-name mtox id))))

(define (do-delete-call call-index)
  (av-hangup my-av call-index)
  (hash-remove! cur-calls call-index))
