#lang racket/gui
; tox.rkt
; contains the tox instances and threads
(require libtoxcore-racket
         "config.rkt"
         "utils.rkt"
         "gui/pass.rkt")

(provide (all-defined-out)
         TOX_PUBLIC_KEY_SIZE
         my-id-bytes)

; proxy options
#;(define my-opts
  (make-Tox-Options (use-ipv6?) (proxy-type) (use-udp?)
                    (proxy-host) (proxy-port) (start-port) (end-port)))
; create a new options struct
(define-values (my-opts opts-err) (tox-options-new))
; set the options struct to its defaults
(cond [(eq? opts-err 'ok)
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
           (define-values (dec-result dec-err decrypted-data)
             (pass-decrypt data-bytes (encryption-pass)))
           (define-values (new-result new-err) (tox-new my-opts decrypted-data))
           (set! my-tox new-result)
           (cond [(eq? new-err 'ok)
                  (send pass-dialog show #f)
                  (displayln "Loading successful!")]
                 [else
                  (let ([mbox (message-box "Blight - Incorrect Passphrase"
                                           "Sorry! That was incorrect.")])
                    (when (eq? mbox 'ok)
                      (displayln "Incorrect password received, trying again.")
                      (send pass-tfield set-value "")))]))
         
         (pass-callback loading-callback)
         (send pass-tfield focus)
         (send pass-dialog show #t)]
        [(zero? (bytes-length data-bytes))
         (let-values ([(new-result new-err) (tox-new my-opts #"")])
           (set! my-tox new-result)
           ; set username
           (set-self-name! my-tox (string->bytes/utf-8 (my-name)))
           ; set status message
           (set-self-status-message! my-tox (string->bytes/utf-8 (my-status-message))))]))

; our AV instance
(define my-av (av-new my-tox 1))

; obtain our tox id
(define my-id-bytes (self-address my-tox))
(define my-id-hex (make-parameter (bytes->hex-string my-id-bytes)))
