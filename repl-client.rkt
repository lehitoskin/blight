#lang racket
; repl-client.rkt
; code straight tooken from rwind
; https://github.com/Metaxal/rwind
(require racket/tcp
         "utils.rkt")

(define client-prompt "blight-repl> ")
(define client-result-format "-> [~a]\n")

(displayln "\n***WARNING! This REPL will grant you FULL ACCESS to the inner workings of Blight.
If you do not know what you are doing, please (exit) immediately!***\n\n")

(print-wait "Trying to connect to server")

(define-values (in out) (tcp-connect/enable-break "localhost" blight-tcp-port))

(displayln "Ok.")

(dynamic-wind
 void
 (λ ()
   (printf client-prompt)(flush-output)
   (define exit? #f)
   (for ([e (in-port read)]
         #:break (or exit? (equal? e '(exit))))
     ; Wrap the output in a list, otherwise it may not be sent/flushed (bug?)
     (write-data/flush e out)
     ; receiving from server, unwrap
     (define res (read in))
     (if (eof-object? res)
         (set! exit? #t)
         (begin
           (printf client-result-format res)
           (printf client-prompt)(flush-output)))))
 (λ ()
   (print-wait "Closing connection")
   (close-input-port in)
   (close-output-port out)
   (displayln "Ok.")))
