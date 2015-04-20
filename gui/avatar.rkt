#lang racket/gui
; avatar.rkt
(require pict
         "../config.rkt"
         "../tox.rkt")

(provide my-avatar)

; create initial avatar bitmap
(define my-avatar (make-parameter (make-bitmap 40 40)))

(define (avatar-lst)
  (for/list ([i (in-directory avatar-dir)]
             #:when (false? (regexp-match #rx"[.]hash" (path->string i)))
             [avatar (in-value i)])
    avatar))

; if we've already set an avatar, load from that file
(let* ([my-pubkey (substring (my-id-hex) 0 (* TOX_PUBLIC_KEY_SIZE 2))]
       [my-avatar-location (build-path avatar-dir (string-append my-pubkey ".png"))])
  (cond [(file-exists? my-avatar-location)
         ; create the bitmap
         (define avatar-bitmap (make-bitmap 40 40))
         ; load the file into the bitmap
         (send avatar-bitmap load-file my-avatar-location)
         ; turn it into a pict
         (define avatar-pict (bitmap avatar-bitmap))
         ; scale the pict to 40x40
         (define avatar-pict-small (scale-to-fit avatar-pict 40 40))
         ; set the avatar to the new one
         (my-avatar (pict->bitmap avatar-pict-small))]))
