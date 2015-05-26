#lang racket/gui
; preferences.rkt
; contains definitions for the preferences dialog box
(require libtoxcore-racket/functions
         "frame.rkt"
         "../blight.rkt"
         "../config.rkt"
         "../tox.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define preferences-box (new dialog%
                             [label "Blight - Edit Preferences"]
                             [style (list 'close-button)]
                             [height 200]
                             [width 400]))

(define tab-panel (new tab-panel%
                       [parent preferences-box]
                       [choices (list "Preferences"
                                      "Proxy")]
                       [callback (λ (l e)
                                   (cond [(zero? (send l get-selection))
                                          (send l delete-child proxy-panel)
                                          (send l add-child pref-panel)]
                                         [else
                                          (send l delete-child pref-panel)
                                          (send l add-child proxy-panel)]))]))

(define pref-panel (new vertical-panel%
                       [parent tab-panel]))

; remove proxy-panel from the window for now
(define proxy-panel (new vertical-panel%
                        [parent tab-panel]
                        [style '(deleted)]))

(define Username_msg (new message%
                          [parent pref-panel]
                          [label "New Username:"]))

;;Define a panel so stuff is aligned
(define User_panel (new horizontal-panel%
                        [parent pref-panel]
                        [alignment '(center center)]))

(define putfield (new text-field%
                      [parent User_panel]
                      [label #f]
                      [style (list  'single)]
                      [callback (λ (l e)
                                  (when (eq? (send e get-event-type)
                                             'text-field-enter)
                                    (let* ([username (send l get-value)]
                                           [name-bytes (string->bytes/utf-8 username)])
                                      ; refuse to set the status if it's empty
                                      (unless (string=? username "")
                                        ; set the new username
                                        (my-name username)
                                        (send username-frame-message set-label username)
                                        (set-self-name! my-tox name-bytes)
                                        (blight-save-data)
                                        (send l set-value "")))))]))

(define putfield-set
  (new button% [parent User_panel]
       [label "Set"]
       [callback (λ (button event)
                   (let* ([username (send putfield get-value)]
                          [name-bytes (string->bytes/utf-8 username)])
                     ; refuse to set the username if it's empty
                     (unless (string=? username "")
                       (my-name username)
                       (send username-frame-message set-label username)
                       (set-self-name! my-tox name-bytes)
                       (blight-save-data)
                       (send putfield set-value ""))))]))

;;Status
(define Status_msg (new message%
                        [parent pref-panel]
                        [label "New Status:"]))

;;Same
(define Status_panel(new horizontal-panel%
                         [parent pref-panel]
                         [alignment '(center center)]))

(define pstfield (new text-field%
                      [parent Status_panel] 
                      [label #f] 
                      [style (list 'single)]
                      [callback (λ (l e)
                                  (let* ([status (send l get-value)]
                                         [status-bytes (string->bytes/utf-8 status)])
                                    (when (eq? (send e get-event-type)
                                               'text-field-enter)
                                      ; refuse to set the status if it's empty
                                      (unless (string=? status "")
                                        ; set the new status
                                        (my-status-message status)
                                        (send status-frame-message set-label status)
                                        (set-self-status-message! my-tox status-bytes)
                                        (blight-save-data)
                                        (send l set-value "")))))]))

(define pstfield-set-button
  (new button%
       [parent Status_panel]
       [label "Set"]
       [callback (λ (button event)
                   (let* ([status (send pstfield get-value)]
                          [status-bytes (string->bytes/utf-8 status)])
                     ; refuse to set status if it's empty
                     (unless (string=? status "")
                       (my-status-message status)
                       (send status-frame-message set-label status)
                       (set-self-status-message! my-tox status-bytes)
                       (blight-save-data)
                       (send pstfield set-value ""))))]))

(define blight-port-start-hpanel
  (new horizontal-panel% [parent pref-panel]))

; blight port bindings
(define blight-port-start-tfield
  (new text-field%
       [parent blight-port-start-hpanel]
       [label "Blight binding start port: "]
       [callback (λ (l e)
                   (let ([val (send e get-value)])
                     (when (and (eq? (send e get-event-type) 'text-field-enter)
                                (not (string=? "" val)))
                       (start-port (string->number val))
                       (send l set-value ""))))]))

(define blight-port-start-button
  (new button%
       [parent blight-port-start-hpanel]
       [label "Set"]
       [callback (λ (button event)
                   (let ([val (send blight-port-start-tfield get-value)])
                     (unless (string=? val "")
                       (start-port (string->number val))
                       (send blight-port-start-tfield set-value ""))))]))

; blight port bindings
(define blight-port-end-hpanel
  (new horizontal-panel% [parent pref-panel]))

(define blight-port-end-tfield
  (new text-field%
       [parent blight-port-end-hpanel]
       [label "Blight binding end port: "]
       [callback (λ (l e)
                   (let ([val (send e get-value)])
                     (when (and (eq? (send e get-event-type) 'text-field-enter)
                                (not (string=? "" val))
                                (> (string->number val) (start-port)))
                       (end-port (string->number val))
                       (send l set-value ""))))]))

(define blight-port-end-button
  (new button%
       [parent blight-port-end-hpanel]
       [label "Set"]
       [callback (λ (button event)
                   (let ([val (send blight-port-end-tfield get-value)])
                     (unless (or (string=? val "") (<= (string->number val) (start-port)))
                       (end-port (string->number val))
                       (send blight-port-end-tfield set-value ""))))]))

; tcp server port
(define blight-tcp-port-hpanel
  (new horizontal-panel% [parent pref-panel]))

(define blight-tcp-port-tfield
  (new text-field%
       [parent blight-tcp-port-hpanel]
       [label "Blight TCP Server port: "]
       [init-value (number->string (tcp-port))]
       [callback (λ (t e)
                   (let ([val (send e get-value)])
                     (when (and (eq? (send e get-event-type) 'text-field-enter)
                                (not (string=? "" val))
                                (> (string->number val) (start-port)))
                       (tcp-port (string->number val))
                       (send t set-value ""))))]))

(define blight-tcp-port-button
  (new button%
       [parent blight-tcp-port-hpanel]
       [label "Set"]
       [callback (λ (button event)
                   (let ([val (send blight-tcp-port-tfield get-value)])
                     (unless (or (string=? val "") (<= (string->number val) (start-port)))
                       (end-port (string->number val))
                       (send blight-tcp-port-tfield set-value ""))))]))

(define checkbox-hpanel
  (new horizontal-panel%
       [parent pref-panel]
       [alignment '(center center)]))

(define ipv6-udp-vpanel
  (new vertical-panel% [parent checkbox-hpanel]))

(define sounds-encrypted-vpanel
  (new vertical-panel% [parent checkbox-hpanel]))

(define ipv6-button (new check-box%
                         [parent ipv6-udp-vpanel]
                         [label "Enable IPv6"]
                         [value (use-ipv6?)]
                         [callback (λ (button event)
                                     (let ([val (send button get-value)])
                                       (use-ipv6? val)
                                       (blight-save-config 'ipv6? val)))]))

(define udp-button (new check-box%
                        [parent ipv6-udp-vpanel]
                        [label "Enable UDP"]
                        [value (use-udp?)]
                        [callback (λ (button event)
                                    (let ([val (send button get-value)])
                                      (use-udp? val)
                                      (blight-save-config 'udp? val)))]))

(define make-sounds-checkbox
  (new check-box%
       [parent sounds-encrypted-vpanel]
       [label "Make sounds"]
       [value (make-noise)]
       [callback (λ (l e)
                   (let ([noise (send l get-value)])
                     (toggle-noise)
                     (blight-save-config 'make-noise noise)))]))

(define encrypted-save-checkbox
  (new check-box%
       [parent sounds-encrypted-vpanel]
       [label "Encrypted save"]
       [value (encrypted?)]
       [callback
        (λ (l e)
          (define enc (send l get-value))
          (cond
            [enc
             (define mbox
               (message-box
                "Blight - Encryption Warning"
                (string-append
                 "WARNING! Encrypting your data file could be dangerous!\n"
                 "If even one byte is incorrect in the saved file,\n"
                 "it will be worthless!")
                #f
                (list 'ok-cancel 'stop)))
             (cond [(eq? mbox 'ok)
                    (define enc-dialog
                      (new dialog%
                           [label "Blight - Encryption Passphrase"]
                           [height 50]
                           [width 400]))
                    (define enc-tfield
                      (new text-field%
                           [parent enc-dialog]
                           [label "New Passphrase: "]
                           [callback (λ (l e)
                                       (when (eq? (send e get-event-type)
                                                  'text-field-enter)
                                         (encryption-pass
                                          (send l get-value))
                                         (send enc-dialog show #f)))]))
                    (define enc-ok-button
                      (new button%
                           [parent enc-dialog]
                           [label "OK"]
                           [callback (λ (button event)
                                       (encryption-pass
                                        (send enc-tfield get-value))
                                       (send enc-dialog show #f))]))
                    (encrypted? enc)
                    (blight-save-config 'encrypted? enc)]
                   [(eq? mbox 'cancel)
                    (send l set-value #f)
                    (encrypted? #f)])]
            [else
             (encrypted? #f)
             (blight-save-config 'encrypted? enc)]))]))

(define change-nospam-button
  (new button%
       [parent pref-panel]
       [label "Change nospam value"]
       [callback (λ (button event)
                   (define mbox (message-box
                                 "Blight - Change nospam"
                                 (string-append "Are you certain you want to"
                                                " change your nospam value?")
                                 #f
                                 (list 'ok-cancel 'stop)))
                   (when (eq? mbox 'ok)
                     (set-self-nospam! my-tox
                                       ; largest (random) can accept
                                       ; corresponds to "FFFFFF2F"
                                       (random 4294967087))
                     ; save our changes
                     (blight-save-data)
                     ; set new tox id
                     (my-id-hex
                      (bytes->hex-string (self-address my-tox)))))]))

; Close button for preferences dialog box
(define preferences-close-button
  (new button%
       [parent pref-panel]
       [label "Close"]
       [callback (λ (button event)
                   (send preferences-box show #f))]))

; proxy options

(define proxy-type-msg
  (new message%
       [parent proxy-panel]
       [label "Note: Proxy Type None will negate the other proxy options."]))

(define proxy-type-choice
  (new choice%
       [parent proxy-panel]
       [label "Proxy Type "]
       [choices '("None" "HTTP" "SOCKS5")]
       [selection 0]))

(define proxy-address-port-panel
  (new horizontal-panel% [parent proxy-panel]))

(define proxy-address-tfield
  (new text-field%
       [parent proxy-address-port-panel]
       [label #f]
       [init-value (if (string=? "" (proxy-host))
                       "example.com"
                       (proxy-host))]
       [min-width 250]))

(define proxy-port-tfield
  (new text-field%
       [parent proxy-address-port-panel]
       [label #f]
       [init-value (if (zero? (proxy-port))
                       "0 ~ 60000"
                       (number->string (proxy-port)))]))

(define proxy-ok-cancel-hpanel
  (new horizontal-panel%
       [parent proxy-panel]
       [alignment '(right center)]))

(define proxy-cancel-button
  (new button%
       [parent proxy-ok-cancel-hpanel]
       [label "Cancel"]
       [callback (λ (button event)
                   ; reset all the old values
                   (send proxy-type-choice set-selection 0)
                   (send proxy-address-tfield set-value (if (string=? "" (proxy-host))
                                                            "example.com"
                                                            (proxy-host)))
                   (send proxy-port-tfield set-value (if (zero? (proxy-port))
                                                         "0 ~ 60000"
                                                         (number->string (proxy-port))))
                   ; close the window
                   (send preferences-box show #f))]))

(define proxy-ok-button
  (new button%
       [parent proxy-ok-cancel-hpanel]
       [label "Save"]
       [callback (λ (button event)
                   ; set all the new values
                   (use-ipv6? (send ipv6-button get-value))
                   (use-udp? (send udp-button get-value))
                   (proxy-type (symbol->string
                                (string-downcase
                                 (send proxy-type-choice get-string-selection))))
                   (proxy-host (send proxy-address-tfield get-value))
                   ; only integers allowed inside port tfield
                   (let ([num (string->number (send proxy-port-tfield get-value))]
                         [port-max 60000])
                     (cond [(and (integer? num) (<= num port-max) (positive? num))
                            (proxy-port num)
                            ; record the new values to the config file
                            (blight-save-config* 'ipv6? (use-ipv6?)
                                                 'udp-disabled? (use-udp?)
                                                 'proxy-type (string->symbol (proxy-type))
                                                 'proxy-address (proxy-host)
                                                 'proxy-port (proxy-port))
                            ; close the window
                            (send preferences-box show #f)]
                           [else
                            (printf "Invalid port number! Valid range: ~a ~~ ~a~n" 0 port-max)
                            (send proxy-port-tfield set-value
                                  (format "~a ~~ ~a" 0 port-max))])))]))
