#lang racket/base
; audio.rkt
; contains stuff to manipulate audio 'n' stuff
(require libopenal-racket)

(provide (all-defined-out)
         close-device!
         destroy-context!
         delete-sources!
         gen-sources
         set-current-context
         stop-source)

(define device (open-device #f))
(define context (create-context device))
(set-current-context context)
