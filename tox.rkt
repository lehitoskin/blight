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
  (make-Tox-Options (use-ipv6?) (use-udp?) (proxy-type) (proxy-host)
                    (proxy-port) (start-port) (end-port) (tcp-port)
                    'none #"" 0))
; create a new options struct
(define-values (my-opts opts-err) (tox-options-new))

(unless (eq? opts-err 'ok)
  (when (make-noise)
    (play-sound (last sounds) #t))
  (raise-result-error 'tox-options-new 'ok opts-err)
  (exit))
; set the options struct to the saved preferences
#|(set-Tox-Options-ipv6?! my-opts (use-ipv6?))
(set-Tox-Options-udp?! my-opts (use-udp?))
(set-Tox-Options-proxy-type! my-opts (proxy-type))
(set-Tox-Options-proxy-host! my-opts (proxy-host))
(set-Tox-Options-proxy-port! my-opts (proxy-port))
(set-Tox-Options-start-port! my-opts (start-port))
(set-Tox-Options-end-port! my-opts (end-port))
(set-Tox-Options-tcp-port! my-opts (tcp-port))|#

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
           (cond [(eq? dec-err 'ok)
                  (set-Tox-Options-save-type! my-opts 'tox-save)
                  (set-Tox-Options-save-data! my-opts decrypted-data)
                  (set-Tox-Options-save-length! my-opts
                                                (bytes-length decrypted-data))]
                 [else (when (make-noise)
                         (play-sound (last sounds) #t))
                       (raise-result-error 'pass-decrypt 'ok dec-err) (exit)])
           (define-values (new-result new-err) (tox-new my-opts))
           (cond [(eq? new-err 'ok)
                  (set! my-tox new-result)
                  (send pass-dialog show #f)
                  (displayln "Loading successful!")
                  ; set our name and status-message from the data we've loaded
                  (let ([name-bytes (self-name my-tox)]
                        [status-msg (self-status-message my-tox)])
                    (my-name (bytes->string/utf-8 name-bytes))
                    (my-status-message (bytes->string/utf-8 status-msg)))]
                 [else
                  (let ([mbox (message-box "Blight - Incorrect Passphrase"
                                           "Sorry! That was incorrect.")])
                    (when (eq? mbox 'ok)
                      (displayln "Incorrect password received, trying again.")
                      (send pass-tfield set-value "")))]))
         
         (pass-callback loading-callback)
         (send pass-tfield focus)
         (send pass-dialog show #t)]
        ; empty data file, start fresh session
        [(zero? (bytes-length data-bytes))
         (define-values (new-result new-err) (tox-new my-opts))
         (set! my-tox new-result)
         ; set username
         (set-self-name! my-tox (string->bytes/utf-8 my-name-default))
         ; set status message
         (set-self-status-message! my-tox
                                   (string->bytes/utf-8 my-status-message-default))]
        [else
         ; load from normal data
         (dprint-wait "Loading data")
         (set-Tox-Options-save-type! my-opts 'tox-save)
         (set-Tox-Options-save-data! my-opts data-bytes)
         (set-Tox-Options-save-length! my-opts (bytes-length data-bytes))
         (define-values (new-result new-err) (tox-new my-opts))
         (cond [(eq? new-err 'ok)
                (set! my-tox new-result)
                (displayln "Ok!")
                ; set our name and status-message from the data we've loaded
                (let ([name-bytes (self-name my-tox)]
                      [status-msg (self-status-message my-tox)])
                  (my-name (bytes->string/utf-8 name-bytes))
                  (my-status-message (bytes->string/utf-8 status-msg)))]
               [else (when (make-noise)
                       (play-sound (last sounds) #t))
                     (raise-result-error 'tox-new "ok" new-err) (exit)])]))

; our AV instance
(define my-av (av-new my-tox 1))

; obtain our tox id
(define my-id-bytes (self-address my-tox))
(define my-id-hex (make-parameter (bytes->hex-string my-id-bytes)))
