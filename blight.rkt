#lang racket/gui
; blight.rkt
(require libtoxcore-racket
         "audio.rkt"
         "config.rkt"
         "history.rkt"
         "tox.rkt"
         "utils.rkt"
         "gui/smart-list.rkt"
         "gui/frame.rkt")

(provide (all-defined-out))

(debug-prefix "Blight: ")
(dprint-wait "Connection to DHT")
; connect to DHT
(cond [(not (false? (bootstrap-from-address my-tox
                                            dht-address
                                            dht-port
                                            dht-public-key)))
       (when make-noise
         (play-sound (fourth sounds) #t))
       (displayln "Connected!")]
      [else (when make-noise
              (play-sound (last sounds) #t))
            (displayln "Connection failed!")])

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
    (debug-prefix "Blight: ")
    (dprint-wait "Saving data")
    (cond [(encrypted?)
           (define size (encrypted-size my-tox))
           (define data-bytes (make-bytes size))
           (define err (encrypted-save! my-tox
                                        data-bytes
                                        encryption-pass))
           (if (zero? err)
               (let ([data-port-out (open-output-file ((data-file))
                                                      #:mode 'binary
                                                      #:exists 'truncate/replace)])
                 (write-bytes data-bytes data-port-out)
                 (close-output-port data-port-out))
               (begin
                 (displayln "There was an error saving the encrypted data!")
                 (when make-noise
                   (play-sound (last sounds) #t))))]
          [else
           (define data-bytes (tox-save my-tox))
           ; SAVE INFORMATION TO DATA
           (let ([data-port-out (open-output-file ((data-file))
                                                  #:mode 'binary
                                                  #:exists 'truncate/replace)])
             (write-bytes data-bytes data-port-out)
             (close-output-port data-port-out))])
    (displayln "Done!")))

; little procedure to wrap things up for us
(define clean-up
  (λ ()
    ; save tox info to data-file
    (blight-save-data)
    ; disconnect from the database
    (disconnect sqlc)
    ; end any calls we might have
    (unless (zero? (get-active-calls my-av))
      (for ([i (get-active-calls my-av)])
        (av-hangup my-av i)))
    ; kill tox threads
    (kill-thread av-loop-thread)
    (kill-thread tox-loop-thread)
    ; kill REPL thread
    (exit-repl)
    ; clean up AL stuff
    ; for buddies
    #;(for ([i (in-range (hash-count cur-buddies))])
      (let ([alsources (contact-data-alsources (hash-ref cur-buddies i))])
        (delete-sources! alsources)))
    ; for groups
    (for ([i (in-range (hash-count cur-groups))])
      (let ([alsources (contact-data-alsources (hash-ref cur-groups i))])
        (unless (false? alsources)
          (delete-sources! alsources))))
    (set-current-context #f)
    (destroy-context! context)
    (close-device! device)
    ; kill av session
    (av-kill! my-av)
    ; this kills the tox
    (tox-kill! my-tox)
    ; log out sound
    (when make-noise
      (play-sound (fifth sounds) #f))))

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
#| ################# END REPL SERVER ################# |#

(define cur-ctx (tox-ctx my-tox my-id-bytes clean-up))

(define (blight-handle-exception unexn)
  (let ([res (show-error-unhandled-exn unexn cur-ctx)])
    (when (eq?  res 'quit)
      (clean-up)
      (exit))))

; tox loop that only uses tox-do and sleeps for some amount of time
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       (call-with-exception-handler
        (λ (exn) (blight-handle-exception exn))
        (λ () (tox-do my-tox)))
       
       (sleep (/ (tox-do-interval my-tox) 1000))
       (loop)))))

; tox av loop
(define av-loop-thread
  (thread
   (λ ()
     (let loop ()
       (call-with-exception-handler
        (λ (exn) (blight-handle-exception exn))
        (λ () (toxav-do my-av)))
       
       (sleep (/ (toxav-do-interval my-av) 1000))
       (loop)))))
