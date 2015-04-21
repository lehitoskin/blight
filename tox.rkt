#lang racket/gui
; tox.rkt
; contains the tox instances and threads
(require libtoxcore-racket
         "config.rkt"
         "utils.rkt")

(provide (all-defined-out)
         TOX_PUBLIC_KEY_SIZE
         my-id-bytes)

; proxy options
#;(define my-opts
  (make-Tox-Options (use-ipv6?) (proxy-type) (use-udp?)
                    (proxy-host) (proxy-port) (start-port) (end-port)))
; create a new options struct
(define-values (my-opts opts-err)
  (let ([opts (tox-options-new)])
    (values (first opts) (second opts))))
; set the options struct to its defaults
(cond [(= (bytes-ref opts-err 0) (_TOX_ERR_OPTIONS_NEW 'OK))
       (tox-options-default my-opts)]
      [else
        (when (make-noise)
          (play-sound (last sounds) #t))
        (error 'tox-options-new "error occurred during allocation: ~s" opts-err)
        (exit)])
; set the options struct to the saved preferences
;(set-Tox-Options-ipv6?! my-opts (use-ipv6?))
;(set-Tox-Options-udp?! my-opts (use-udp?))
;(set-Tox-Options-proxy-type! my-opts (proxy-type))
;(set-Tox-Options-proxy-host! my-opts (proxy-host))
;(set-Tox-Options-proxy-port! my-opts (proxy-port))
;(set-Tox-Options-start-port! my-opts (start-port))
;(set-Tox-Options-end-port! my-opts (end-port))

; av settings
(define my-csettings DefaultCSettings)
; instantiate Tox session
(define my-tox #f)
; is this kosher?
; beats asking for the pass every time we save...
(define encryption-pass (make-parameter ""))

(let ([data-bytes (file->bytes ((data-file)) #:mode 'binary)])
  (debug-prefix "Blight: ")
  ; data file is encrypted, decrypt it first
  (cond [(and (not (zero? (bytes-length data-bytes)))
              (data-encrypted? data-bytes))
         ; we've got an encrypted file, we should save it as encrypted
         (encrypted? #t)
         ; ask the user what the password is
         (dprint-wait "Loading encrypted data")
         (define (loading-callback)
           (encryption-pass (send pass-tfield get-value))
           (let* ([decrypted-result (pass-decrypt data-bytes (encryption-pass))]
                  [decrypted-data (last decrypted-result)]
                  [new-result (tox-new my-opts decrypted-data)]
                  [new-err (second new-result)])
             (set! my-tox (first new-result))
             (cond [(zero? (bytes-ref new-err 0))
                    (send pass-dialog show #f)
                    (displayln "Loading successful!")]
                   [else
                    (let ([mbox (message-box "Blight - Incorrect Passphrase"
                                             "Sorry! That was incorrect.")])
                      (when (eq? mbox 'ok)
                        (displayln "Incorrect password received, trying again.")
                        (send pass-tfield set-value "")))])))
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
                            (when (eq? (send e get-event-type) 'text-field-enter)
                              (loading-callback)))]))
         (define pass-hpanel
           (new horizontal-panel%
                [parent pass-dialog]
                [alignment '(right center)]))
         (define pass-cancel-button
           (new button%
                [label "Cancel"]
                [parent pass-hpanel]
                [callback (λ (button event)
                            (exit))]))
         (define pass-ok-button
           (new button%
                [label "OK"]
                [parent pass-hpanel]
                [callback (λ (button event)
                            (loading-callback))]))
         (send pass-dialog show #t)]
        [(zero? (bytes-length data-bytes))
         (set! my-tox (car (tox-new my-opts #"")))
         ; set username
         (set-self-name! my-tox (string->bytes/utf-8 (my-name)))
         ; set status message
         (set-self-status-message! my-tox (string->bytes/utf-8 (my-status-message)))]))

; our AV instance
(define my-av (av-new my-tox 1))

; obtain our tox id
(define my-id-bytes (self-address my-tox))
(define my-id-hex (make-parameter (bytes->hex-string my-id-bytes)))
