#lang racket/base
; repl.rkt
(require racket/tcp
         racket/port
         "utils.rkt")
(provide (all-defined-out))

#| ################# BEGIN REPL SERVER ################# |#
; code straight tooken from rwind
; https://github.com/Metaxal/rwind
(define-namespace-anchor server-namespace-anchor)

(define server-namespace (namespace-anchor->namespace server-namespace-anchor))

(define (start-blight-repl [continuous? #t])
  (dprint-wait "Opening listener")
  (define listener (tcp-listen blight-tcp-port 4 #t "127.0.0.1"))
  (dprint-ok)
  (dynamic-wind
   void
   (λ ()
     (let accept-loop ()
       (dprint-wait "Waiting for client")
       (define-values (in out) (tcp-accept/enable-break listener))
       (printf "Client is connected.\n")
       (dynamic-wind
        void
        (λ ()
          (dprint-wait "Waiting for data")
          (for ([e (in-port read in)]
                #:break (equal? e '(exit)))
            (printf "Received ~a\n" e)
            ; if it fails, simply return the message
            (with-handlers ([exn:fail? (λ (e)
                                         (define res (exn-message e))
                                         (dprintf "Sending exception: ~a" res)
                                         (write-data/flush res out))])
              (define res
                (begin
                  (dynamic-wind
                   void
                   (λ ()
                     (with-output-to-string
                      (λ ()
                        (define r (eval e server-namespace))
                        (unless (void? r)
                          (write r)))))
                   void)))
              (dprint-wait "Sending value: ~a" res)
              ; Printed in a string, to send a string,
              ; because the reader cannot read things like #<some-object>
              (write-data/flush res out))
            (dprint-ok)
            (dprint-wait "Waiting for data")))
        (λ ()
          (dprintf "Closing connection.\n")
          (close-input-port in)
          (close-output-port out)
          (when continuous?
            (accept-loop))))))
   ; out
   (λ ()
     (dprint-wait "Closing listener")
     (tcp-close listener)
     (dprint-ok))))

(define repl-thread #f)

(define (init-repl)
  ;; Start the server
  (set! repl-thread
        (parameterize ([debug-prefix "Srv: "])
          (thread start-blight-repl))))

(define (exit-repl)
  ; Call a break so that dynamic-wind can close the ports and the listener
  ;(break-thread server-thread)
  (kill-thread repl-thread))
