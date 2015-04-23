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
(let-values ([(result err) (tox-bootstrap my-tox
                                          (dht-address)
                                          (dht-port)
                                          (dht-public-key))])
  (cond [(not (false? result))
         (when (make-noise)
           (play-sound (fourth sounds) #t))
         (displayln "Connected!")]
        [else (when (make-noise)
                (play-sound (last sounds) #t))
              (printf "Connection failed! Error code: ~s\n" err)]))

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
    (debug-prefix "Blight: ")
    (dprint-wait "Saving data")
    (cond [(encrypted?)
           
           ; allow an option to change the password every time?
           (when (string=? (encryption-pass) "")
             (define pass-dialog (new dialog%
                                      [label "Blight - Enter Passphrase"]
                                      [height 50]
                                      [width 400]
                                      [style (list 'close-button)]))
             (define pass-tfield
               (new text-field%
                    [label "Enter Passphrase: "]
                    [parent pass-dialog]
                    [style '(single password)]
                    [callback (λ (l e)
                                (when (and (eq? (send e get-event-type) 'text-field-enter)
                                           (not (string=? (send l get-value) "")))
                                  (encryption-pass (send l get-value))
                                  (send pass-dialog show #f)))]))
             (define pass-ok-button
               (new button%
                    [label "OK"]
                    [parent pass-dialog]
                    [callback (λ (button event)
                                (when (not (string=? (send pass-tfield get-value) ""))
                                  (encryption-pass (send pass-tfield get-value))
                                  (send pass-dialog show #f)))]))
             (send pass-dialog show #t))
           
           ; data to encrypt
           (define data-bytes (savedata my-tox))
           ; encrypt the data to be saved
           (define-values (enc-success enc-err encrypted-data)
             (pass-encrypt data-bytes (encryption-pass)))
           (if enc-success
               (let ([data-port-out (open-output-file ((data-file))
                                                      #:mode 'binary
                                                      #:exists 'truncate/replace)])
                 (write-bytes encrypted-data data-port-out)
                 (close-output-port data-port-out))
               (begin
                 (printf "There was an error saving the encrypted data! ~s\n" enc-err)
                 (when (make-noise)
                   (play-sound (last sounds) #t))))]
          [else
           (define data-bytes (savedata my-tox))
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
    (when (eq? res 'quit)
      (clean-up)
      (exit))))

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
