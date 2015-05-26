#lang racket/gui
; blight.rkt
(require libtoxcore-racket
         "audio.rkt"
         "config.rkt"
         "history.rkt"
         "tox.rkt"
         "utils.rkt"
         "gui/smart-list.rkt"
         "gui/frame.rkt"
         "gui/pass.rkt")

(provide (all-defined-out))

; connect to DHT for the first time
(parameterize ([debug-prefix "Blight: "])
  (dprint-wait "Connecting to DHT")
  (let-values ([(result err) (tox-bootstrap my-tox
                                            (dht-node-address (first node-list))
                                            (dht-node-port (first node-list))
                                            (dht-node-public-key (first node-list)))])
    (cond [(not (false? result))
           (when (make-noise)
             (play-sound (fourth sounds) #t))
           (displayln "Connected!")]
          [else (when (make-noise)
                  (play-sound (last sounds) #t))
                (printf "Connection failed! Error code: ~s\n" err)])))

; reusable procedure to save tox information to data-file
(define (blight-save-data)
  (debug-prefix "Blight: ")
  (dprint-wait "Saving data")
  (cond [(encrypted?)
         
         ; allow an option to change the password every time?
         (when (string=? (encryption-pass) "")
           (define (new-pass)
             (encryption-pass (send pass-tfield get-value))
             (send pass-dialog show #f)
             (send pass-tfield set-value ""))
           
           (pass-callback new-pass)
           
           (send pass-tfield focus)
           (send pass-dialog show #t))
         
         ; data to encrypt
         (define data-bytes (savedata my-tox))
         ; encrypt the data to be saved
         (define-values (enc-success enc-err encrypted-data)
           (pass-encrypt data-bytes (encryption-pass)))
         (cond [enc-success
                (with-output-to-file ((data-file))
                  (λ () (write-bytes encrypted-data))
                  #:mode 'binary
                  #:exists 'truncate/replace)]
               [else
                (printf "There was an error saving the encrypted data! ~s\n" enc-err)
                (when (make-noise)
                  (play-sound (last sounds) #t))])]
        [else
         (define data-bytes (savedata my-tox))
         ; save tox session to file
         (with-output-to-file ((data-file))
           (λ () (write-bytes data-bytes))
           #:mode 'binary
           #:exists 'truncate/replace)])
  (displayln "Done!"))

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
    ; kill threads
    (kill-thread (bootstrap-thread))
    (kill-thread av-loop-thread)
    (kill-thread tox-loop-thread)
    ; kill REPL thread
    (exit-repl)
    ; clean up AL stuff
    ; for buddies
    (for ([i (in-range (hash-count cur-calls))])
      (let ([alsource (friend-call-alsource (hash-ref cur-calls i))])
        (delete-sources! (list alsource))))
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
    ; free options' space
    (tox-options-free my-opts)
    ; log out sound
    (when (make-noise)
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
                        #|(define r (eval e server-namespace))
                        (unless (void? r)
                          (write r))|#
                        (for-each (λ (retval)
                                    (unless (void? retval)
                                      (write retval)
                                      (newline)))
                                  (call-with-values (λ () (eval e server-namespace)) list)))))
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
    (when (eq? res 'quit)
      (clean-up)
      (exit))))

(define bootstrapper
  (λ ()
    (parameterize ([debug-prefix "Blight: "])
      (let loop ()
        ; bootstrap to 4 nodes at a time
        (for ([i (in-list node-list)])
          (define nick (dht-node-nick i))
          (define address (dht-node-address i))
          (define port (dht-node-port i))
          (define pubkey (dht-node-public-key i))
          
          (define-values (result err) (tox-bootstrap my-tox address port pubkey))
          
          (if (false? result)
              (dprintf "Error connecting to node ~a: ~a~n" nick err)
              (dprintf "Bootstrapped to ~a~n" nick)))
        (sleep 5)
        (when (eq? (self-connection-status my-tox) 'none)
          (loop))))))

(define bootstrap-thread (make-parameter (thread bootstrapper)))

; tox loop that only uses iterate and sleeps for some amount of time
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       (call-with-exception-handler
        (λ (exn) (blight-handle-exception exn))
        (λ () (iterate my-tox)))
       
       (sleep (/ (iteration-interval my-tox) 1000))
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
