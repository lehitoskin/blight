#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client written in Racket
(require libtoxcore-racket ; wrapper
         "chat.rkt"         ; contains definitions for chat window
         "config.rkt"       ; default config file
         "number-conversions.rkt" ; bin, dec, and hex conversions
         "helpers.rkt"      ; various useful functions
         ffi/unsafe         ; needed for neat pointer shenanigans
         json               ; for reading and writing to config file
         "history.rkt"      ; access sqlite db for stored history
         "toxdns.rkt")      ; for toxdns lookups

(define license-message
  "Blight - a Tox client written in Racket.
Copyright (C) 2014 Lehi Toskin.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.


Tox's sounds are licensed under the \"Creative Commons Attribution 3.0
Unported\", all credit attributed to Adam Reid.")

; instantiate Tox session
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))

#| ############ BEGIN JSON STUFF ############ |#
; read from blight-config.json
(define json-info (read-json config-port-in))
; set variables to values those contained in blight-config.json
(define dht-address (hash-ref json-info 'dht-address))
(define dht-port (hash-ref json-info 'dht-port))
(define dht-public-key (hash-ref json-info 'dht-public-key))
(define my-name (hash-ref json-info 'my-name-last))
(define my-status-message (hash-ref json-info 'my-status-last))
(define make-noise (hash-ref json-info 'make-noise-last))

#|
reusable procedure to save information to blight-config.json

1. read from blight-config.json to get the most up-to-date info
2. modify the hash
3. save the modified hash to blight-config.json

key is a symbol corresponding to the key in the hash
val is a value that corresponds to the value of the key
|#
(define blight-save-config
  (λ (key val)
    (let* ((new-input-port (open-input-file config-file
                                            #:mode 'text))
           (json (read-json new-input-port))
           (modified-json (hash-set* json key val))
           (config-port-out (open-output-file config-file
                                              #:mode 'text
                                              #:exists 'truncate/replace)))
      (json-null 'null)
      (write-json modified-json config-port-out)
      (write-json (json-null) config-port-out)
      (close-input-port new-input-port)
      (close-output-port config-port-out))))

#| ############ BEGIN TOX STUFF ############ |#
; we have 0 transfers right now
(define rtransfers null)

; data-file is empty, use default settings
(cond [(zero? (file-size data-file))
       ; set username
       (tox_set_name my-tox my-name (bytes-length
                                     (string->bytes/utf-8 my-name)))
       ; set status message
       (tox_set_status_message my-tox my-status-message
                               (bytes-length
                                (string->bytes/utf-8 my-status-message)))]
      ; data-file is not empty, load from data-file
      [(not (zero? (file-size data-file)))
       ; load the messenger from data of size length
       (define size (file-size data-file))
       (define data-ptr (malloc 'atomic size))
       ; no conversions necessary because bytes-ref reports a decimal value
       (let ((my-bytes (file->bytes data-file #:mode 'binary)))
         (do ((i 0 (+ i 1)))
           ((= i size))
           (ptr-set! data-ptr _uint8_t i (bytes-ref my-bytes i))))
       (printf "Loading from data file... ~a\n" (tox_load my-tox data-ptr size))])

; obtain our tox id
(define my-id-bytes (malloc 'atomic
                            (* TOX_FRIEND_ADDRESS_SIZE
                               (ctype-sizeof _uint8_t))))
(tox_get_address my-tox my-id-bytes)
(define my-id-hex (ptrtox->hextox my-id-bytes TOX_FRIEND_ADDRESS_SIZE))

; connect to DHT
(display "Connecting to network... ")
(cond [(= (tox_bootstrap_from_address my-tox
                                      dht-address
                                      TOX_ENABLE_IPV6_DEFAULT
                                      dht-port
                                      dht-public-key)
          1)
       (unless (false? make-noise)
         (play-sound (fourth sounds) #t))
       (displayln "Connected!")]
      [else (unless (false? make-noise)
              (play-sound (last sounds) #t))
            (displayln "Did not connect!")])

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
    (thread
     (λ ()
       ; necessary for saving the messenger
       (define size (tox_size my-tox))
       (define data-ptr (malloc 'atomic size))
       ; place all tox info into data-ptr
       (tox_save my-tox data-ptr)
       ; SAVE INFORMATION TO DATA
       (let ((my-data #"")
             (data-port-out (open-output-file data-file
                                              #:mode 'binary
                                              #:exists 'truncate/replace)))
         (do ((i 0 (+ i 1)))
           ((= i size))
           (set! my-data
                 (bytes-append my-data
                               (bytes (ptr-ref data-ptr _uint8_t i)))))
         (write-bytes my-data data-port-out)
         (close-output-port data-port-out))))))

; little procedure to wrap things up for us
(define clean-up
  (λ ()
    ; save tox info to data-file
    (define data-thread (blight-save-data))
    ; disconnect from the database
    (disconnect sqlc)
    ; close config file input port
    (close-input-port config-port-in)
    ; kill tox thread
    (kill-thread tox-loop-thread)
    ; this kills the tox
    (tox_kill my-tox)
    ; log out sound
    (unless (false? make-noise)
      (play-sound (fifth sounds) #f))
    ; make sure the data is completely saved
    (let loop ()
      (unless (not (thread-running? data-thread))
        (sleep 1/4)
        (loop)))))

#| ############### BEGIN GUI STUFF ################## |#
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight - Friend List"]
                   [width 400]
                   [height 300]
                   [x 0]
                   [y 0]))

; make a static text message in the frame
(define frame-msg (new message%
                       [parent frame]
                       [label "Blight Friend List"]
                       [min-width 40]))

(define username-frame-message (new message%
                                    [parent frame]
                                    [label my-name]
                                    [min-width
                                     (bytes-length
                                      (string->bytes/utf-8 my-name))]))
(send username-frame-message auto-resize #t)

(define status-frame-message (new message%
                                  [parent frame]
                                  [label my-status-message]
                                  [min-width
                                   (bytes-length
                                    (string->bytes/utf-8 my-status-message))]))
(send status-frame-message auto-resize #t)

; choices for status type changes
(define status-choice
  (new choice%
       [parent frame]
       [label ""]
       [min-width 400]
       [choices '("Available"
                  "Away"
                  "Busy")]
       [callback (λ (l e)
                   (cond [(= (send l get-selection) (_TOX_USERSTATUS-index 'NONE))
                          (tox_set_user_status my-tox (_TOX_USERSTATUS-index 'NONE))]
                         [(= (send l get-selection) (_TOX_USERSTATUS-index 'AWAY))
                          (tox_set_user_status my-tox (_TOX_USERSTATUS-index 'AWAY))]
                         [(= (send l get-selection) (_TOX_USERSTATUS-index 'BUSY))
                          (tox_set_user_status my-tox (_TOX_USERSTATUS-index 'BUSY))]))]))

#| ################## FRIEND LIST STUFF #################### |#
; obtain number of friends
(define initial-num-friends (tox_count_friendlist my-tox))

; we want at least one chat window
(define friend-list (list (new chat-window%
                               [this-label "a"]
                               [this-width 400]
                               [this-height 600]
                               [this-tox my-tox])))

; loop through and create as many chat-window%'s
; as there are friends and add them to the gvector
(unless (zero? initial-num-friends)
  (do ((i 1 (+ i 1)))
    ((= i initial-num-friends))
    (let ((new-window (new chat-window%
                           [this-label "a"]
                           [this-width 400]
                           [this-height 600]
                           [this-tox my-tox])))
      (set! friend-list (append friend-list (list new-window))))))

; list box for friend list
; format: (indexed by list-box starting from 0)
;  choice -> string -> username
;  data -> string -> tox public key
(define list-box
  (new list-box%
       [label "Select Friend"]
       [parent frame]
       [min-height 250]
       [style (list 'single 'vertical-label)]
       [choices (list "Test")]
       [callback (λ (l e)
                   ; when double click, open the window
                   (when (eq? (send e get-event-type)
                              'list-box-dclick)
                     (let* ((friend-name (send list-box get-string
                                               (send list-box get-selection)))
                            (friend-key (send list-box get-data
                                              (send list-box get-selection))))
                       ; look for the friend's name and key in the list
                       ; and associate the open window with the friend's name
                       (define friend-name-checker
                         (λ (num)
                           (if (= num (length friend-list))
                               (list-ref friend-list (- (length friend-list) 1))
                               (cond [(and
                                       ; check friend name
                                       (string=?
                                        (substring friend-name 4)
                                        (send (list-ref friend-list num) get-name))
                                       ; check friend public key
                                       (string=?
                                        friend-key
                                        (send (list-ref friend-list num) get-key)))
                                      ; return the chat window
                                      (list-ref friend-list num)]
                                     ; otherwise, keep looping
                                     [else (friend-name-checker (+ num 1))]))))
                       (define friend-window (friend-name-checker 0))
                       ; check if we're already chatting
                       (unless (send friend-window is-shown?)
                         (send friend-window show #t)))))]))

; set data for each item in list-box
; data may be arbitrary, but a label will suffice
(send list-box set-data 0 "0123456789ABCDEF")

; helper to avoid spamming notification sounds
(define status-checker
  (λ (friendnumber status)
    (let ((type (tox_get_user_status my-tox friendnumber)))
      (cond [(zero? status)
             ; if the user is offline, prepend his name with (X)
             (send list-box set-string friendnumber
                   (string-append
                    "(X) "
                    (send (list-ref friend-list friendnumber) get-name)))]
            ; user is online, check his status type
            [else (on-status-type-change my-tox friendnumber type #f)]))))

; nuke list-box and repopulate it
(define update-list-box
  (λ ()
    ; get current number of friends
    (define num-friends (tox_count_friendlist my-tox))
    (unless (zero? num-friends)
      (send list-box clear)
      (define friend-name-bytes (malloc 'atomic (* TOX_FRIEND_ADDRESS_SIZE
                                                   (ctype-sizeof _uint8_t))))
      (define friend-key-bytes (malloc 'atomic (* TOX_CLIENT_ID_SIZE
                                                  (ctype-sizeof _uint8_t))))
      ; loop until we get all our friends
      (do ((window-num 0 (+ window-num 1)))
        ((= window-num num-friends))
        (let* ((friend-name-text "")
               (friend-name-length (tox_get_name my-tox window-num friend-name-bytes)))
          ; grab our friend's public key
          (tox_get_client_id my-tox window-num friend-key-bytes)
          (define friend-key-text (ptrtox->hextox friend-key-bytes TOX_CLIENT_ID_SIZE))
          (define friend-num (tox_get_friend_number my-tox friend-key-bytes))
          ; grab our friends' name into the pointer
          ; loop through and add it to friend-name-text
          (do ((ptrnum 0 (+ ptrnum 1)))
            ((= ptrnum friend-name-length))
            (set! friend-name-text
                  (string-append friend-name-text
                                 (string
                                  (integer->char
                                   (ptr-ref friend-name-bytes _uint8_t ptrnum))))))
          ; add to the friend list
          (send list-box append (string-append "(X) " friend-name-text) friend-key-text)
          ; make sure friend numbering is correct
          (send (list-ref friend-list window-num) set-friend-num friend-num)
          ; add to the gvector
          (send (list-ref friend-list window-num) set-name friend-name-text)
          (send (list-ref friend-list window-num) set-key friend-key-text)
          (send (list-ref friend-list window-num) set-new-label
                (string-append "Blight - " friend-name-text))
          ; update our friends' status icon
          (status-checker window-num (tox_get_friend_connection_status my-tox window-num)))))))
(update-list-box)

; panel for choice and buttons
(define panel (new horizontal-panel%
                   [parent frame]
                   [alignment (list 'right 'center)]))

; dialog box when exiting
(define exit-dialog (new dialog%
                         [label "Exit Blight"]
                         [style (list 'close-button)]))

; dialog box when looking at Help
(define help-dialog (new dialog%
                         [label "About Blight"]
                         [style (list 'close-button)]))

(define help-text (new text%
                       [line-spacing 1.0]
                       [auto-wrap #t]))
(send help-text change-style font-size-delta)
(send help-text insert license-message)

; canvas to print the license message
(define help-editor-canvas (new editor-canvas%
                                [parent help-dialog]
                                [min-height 380]
                                [min-width 600]
                                [vert-margin 10]
                                [editor help-text]
                                [style (list 'control-border 'no-hscroll
                                             'auto-vscroll 'no-focus)]))

; button to close the About Blight window
(define help-about-ok
  (new button%
       [parent help-dialog]
       [label "&OK"]
       [callback (λ (button event)
                   (send help-dialog show #f))]))

#| ############ MENU BAR AND STUFF ############## |#
; menu bar for the frame
(define frame-menu-bar (new menu-bar%
                            [parent frame]))

; menu File for menu bar
(define menu-file (new menu%
                       [parent frame-menu-bar]
                       [label "&File"]
                       [help-string "Open, Quit, etc."]))

; Copy ID to Clipboard item for File
(define menu-copy-id
  (new menu-item%
       [parent menu-file]
       [label "Copy ID to Clipboard"]
       [help-string "Copies your Tox ID to the clipboard"]
       [callback (λ (button event)
                   ; copy id to clipboard
                   (send chat-clipboard set-clipboard-string
                         my-id-hex
                         (current-seconds)))]))

; Quit menu item for File
; uses message-box with 'ok-cancel
(define menu-quit
  (new menu-item%
       [parent menu-file]
       [label "&Quit"]
       [shortcut #\Q]
       [help-string "Quit Blight"]
       [callback (λ (button event)
                   ;(send exit-dialog show #t))])
                   (let ((mbox (message-box "Blight - Quit Blight"
                                            "Are you sure you want to quit Blight?"
                                            exit-dialog
                                            (list 'ok-cancel 'caution))))
                     (if (eq? mbox 'ok)
                         ((clean-up)
                          (exit))
                         null)))]))

; menu Edit for menu bar
(define menu-edit (new menu%
                       [parent frame-menu-bar]
                       [label "&Edit"]
                       [help-string "Modify Blight"]))

#| #################### PREFERENCES STUFF ################### |#
(define preferences-box (new dialog%
                             [label "Blight - Edit Preferences"]
                             [style (list 'close-button)]
                             [height 200]
                             [width 400]))

(define Username_msg (new message%
                          [parent preferences-box]
                          [label "New Username:"]))

;;Define a panel so stuff is aligned
(define User_panel (new horizontal-panel%
                        [parent preferences-box]
                        [alignment '(center center)]))

(define putfield (new text-field%
                      [parent User_panel]
                      [label ""]
                      [style (list  'single)]
                      [callback (λ (l e)
                                  (let ((username (send l get-value)))
                                    (when (eq? (send e get-event-type)
                                               'text-field-enter)
                                      ; refuse to set the status if it's empty
                                      (unless (string=? username "")
                                        ; set the new username
                                        (blight-save-config 'my-name-last username)
                                        (send username-frame-message set-label username)
                                        (tox_set_name my-tox username
                                                      (bytes-length
                                                       (string->bytes/utf-8 username)))
                                        (blight-save-data)
                                        (send l set-value "")))))]))

(define putfield-set
  (new button% [parent User_panel]
       [label "Set"]
       [callback (λ (button event)
                   (let ((username (send putfield get-value)))
                     ; refuse to set the username if it's empty
                     (unless (string=? username "")
                       (blight-save-config 'my-name-last username)
                       (send username-frame-message set-label username)
                       (tox_set_name my-tox username
                                     (bytes-length
                                      (string->bytes/utf-8 username)))
                       (blight-save-data)
                       (send putfield set-value ""))))]))

;;Status
(define Status_msg (new message%
                        [parent preferences-box]
                        [label "New Status:"]))

;;Same
(define Status_panel(new horizontal-panel%
                         [parent preferences-box]
                         [alignment '(center center)]))

(define pstfield (new text-field%
                      [parent Status_panel] 
                      [label ""] 
                      [style (list 'single)]
                      [callback (λ (l e)
                                  (let ((status (send l get-value)))
                                    (when (eq? (send e get-event-type)
                                               'text-field-enter)
                                      ; refuse to set the status if it's empty
                                      (unless (string=? status "")
                                        ; set the new status
                                        (blight-save-config 'my-status-last status)
                                        (send status-frame-message set-label status)
                                        (tox_set_status_message my-tox status
                                                                (bytes-length
                                                                 (string->bytes/utf-8 status)))
                                        (blight-save-data)
                                        (send l set-value "")))))]))

(define pstfield-set-button
  (new button%
       [parent Status_panel]
       [label "Set"]
       [callback (λ (button event)
                   (let ((status (send pstfield get-value)))
                     ; refuse to set status if it's empty
                     (unless (string=? status "")
                       (blight-save-config 'my-status-last status)
                       (send status-frame-message set-label status)
                       (tox_set_status_message my-tox status 
                                               (bytes-length
                                                (string->bytes/utf-8 status)))
                       (blight-save-data)
                       (send pstfield set-value ""))))]))

(define change-nospam-button
  (new button%
       [parent preferences-box]
       [label "Change nospam value"]
       [callback (λ (button event)
                   (let ((mbox (message-box "Blight - Change nospam"
                                            (string-append "Are you certain you want to"
                                                           " change your nospam value?")
                                            #f
                                            (list 'ok-cancel 'stop))))
                     (when (eq? mbox 'ok)
                       (tox_set_nospam my-tox
                                       ; largest (random) can accept
                                       ; corresponds to "FFFFFF2F"
                                       (random 4294967087))
                       ; save our changes
                       (blight-save-data)
                       ; set new tox id
                       (tox_get_address my-tox my-id-bytes)
                       (set! my-id-hex
                             (ptrtox->hextox my-id-bytes TOX_FRIEND_ADDRESS_SIZE)))))]))

(define make-sounds-button
  (new check-box%
       [parent preferences-box]
       [label "Make sounds"]
       [value (not (false? make-noise))]
       [callback (λ (l e)
                   (let ((noise (send l get-value)))
                     (set! make-noise noise)
                     (blight-save-config 'make-noise-last noise)))]))

; Close button for preferences dialog box
(define preferences-close-button
  (new button%
       [parent preferences-box]
       [label "Close"]
       [callback (λ (button event)
                   (send preferences-box show #f))]))
#| #################### END PREFERENCES STUFF ################### |#

#| #################### BEGIN ADD FRIEND STUFF ####################### |#
(define add-friend-box (new dialog%
                            [label "Blight - Add a new Tox friend"]
                            [style (list 'close-button)]))

(define dns-msg (new message%
                     [parent add-friend-box]
                     [label "DNS nickname:"]))

(define dns-panel (new horizontal-panel%
                       [parent add-friend-box]
                       [alignment '(center center)]))

(define add-friend-txt-tfield (new text-field%
                                   [parent dns-panel]
                                   [label ""]
                                   [min-width 38]))

; choices for status type changes
(define dns-domain-choice
  (new choice%
       [parent dns-panel]
       [label ""]
       [choices '("toxme.se"
                  "utox.org")]))

(define hex-message (new message%
                         [parent add-friend-box]
                         [label "Friend ID(X):"]))

(define hex-panel (new horizontal-panel%
                       [parent add-friend-box]
                       [alignment '(center center)]))

; add friend with Tox ID
(define add-friend-hex-tfield (new text-field%
                                   [parent hex-panel]
                                   [label ""]
                                   [min-width 38]
                                   [callback (λ (l e)
                                               (if (tox-id? (send l get-value))
                                                   (send hex-message set-label
                                                         "Friend ID(✓):")
                                                   (send hex-message set-label
                                                         "Friend ID(X):")))]))

(define message-message (new message%
                             [parent add-friend-box]
                             [label "Message:"]))

(define message-panel (new horizontal-panel%
                           [parent add-friend-box]
                           [alignment '(center center)]))

; message to send as a friend request
(define add-friend-message-tfield (new text-field%
                                       [parent message-panel]
                                       [label ""]
                                       [min-width 38]
                                       [init-value "Please let me add you to my contact list"]))

; panel for the buttons
(define add-friend-panel (new horizontal-panel%
                              [parent add-friend-box]
                              [alignment '(right center)]))

(define add-friend-error-dialog (new dialog%
                                     [label "Invalid Tox ID"]
                                     [style (list 'close-button)]))

; don't actually want to add a friend right now
(define add-friend-cancel-button
  (new button%
       [parent add-friend-panel]
       [label "Cancel"]
       [callback (λ (button event)
                   (send add-friend-hex-tfield set-value "")
		   (send add-friend-txt-tfield set-value "")
                   (send add-friend-box show #f))]))

; OK button for add-friend dialog box
(define add-friend-ok-button
  (new button%
       [parent add-friend-panel]
       [label "OK"]
       [callback (λ (button event)
                   (let ((nick-tfield (send add-friend-txt-tfield get-value))
                         (hex-tfield (send add-friend-hex-tfield get-value))
                         (message-tfield (send add-friend-message-tfield get-value))
                         (domain (send dns-domain-choice get-string
                                       (send dns-domain-choice get-selection))))
                     ; add the friend to the friend list
                     (cond [(or
                             (and (string=? hex-tfield "")
                                  (not (string=? nick-tfield "")))
                             (and (string=? nick-tfield "")
                                  (tox-id? hex-tfield)))
                            ; convert hex to bytes
                            (define nick-bytes (malloc 'atomic
                                                       (* TOX_FRIEND_ADDRESS_SIZE
                                                          (ctype-sizeof _uint8_t))))
                            (cond [(string=? nick-tfield "")
                                   (do ((i 0 (+ i 1))
                                        (j 0 (+ j 2)))
                                     ((= i TOX_FRIEND_ADDRESS_SIZE))
                                     (ptr-set! nick-bytes _uint8_t i
                                               (hex->dec
                                                (string-append
                                                 (string (string-ref hex-tfield j))
                                                 (string (string-ref hex-tfield (+ j 1)))))))]
                                  [(string=? hex-tfield "")
                                   (define friend-hex (tox-dns1 nick-tfield domain))
                                   (do ((i 0 (+ i 1))
                                        (j 0 (+ j 2)))
                                     ((= i TOX_FRIEND_ADDRESS_SIZE))
                                     (ptr-set! nick-bytes _uint8_t i
                                               (hex->dec
                                                (string-append
                                                 (string (string-ref friend-hex j))
                                                 (string (string-ref friend-hex (+ j 1)))))))])
                            (let ((err (tox_add_friend my-tox
                                                       nick-bytes
                                                       message-tfield
                                                       (bytes-length
                                                        (string->bytes/utf-8 message-tfield)))))
                              ; check for all the friend add errors
                              (cond [(= err (_TOX_FAERR-index 'TOOLONG))
                                     (displayln "ERROR: TOX_FAERR_TOOLONG")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'NOMESSAGE))
                                     (displayln "ERROR: TOX_FAERR_NOMESSAGE")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'OWNKEY))
                                     (displayln "ERROR: TOX_FAERR_OWNKEY")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'ALREADYSENT))
                                     (displayln "ERROR: TOX_FAERR_ALREADYSENT")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'UNKNOWN))
                                     (displayln "ERROR: TOX_FAERR_UNKNOWN")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'BADCHECKSUM))
                                     (displayln "ERROR: TOX_FAERR_BADCHECKSUM")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'SETNEWNOSPAM))
                                     (displayln "ERROR: TOX_FAERR_SETNEWNOSPAM")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_FAERR-index 'NOMEM))
                                     (displayln "ERROR: TOX_FAERR_NOMEM")
                                     (unless (false? make-noise)
                                       (play-sound (last sounds) #t))]
                                    [else (displayln "All okay!")
                                          ; append new friend to the gvector
                                          (set! friend-list (append
                                                             friend-list
                                                             (list (new chat-window%
                                                                        [this-label "a"]
                                                                        [this-width 400]
                                                                        [this-height 600]
                                                                        [this-tox my-tox]))))
                                          ; save the tox data
                                          (blight-save-data)
                                          ; update friend list, but don't mess up
                                          ; the numbering we already have
                                          (update-list-box)
                                          ; zero-out some fields
                                          (send add-friend-hex-tfield set-value "")
                                          (send add-friend-txt-tfield set-value "")
                                          ; close the window
                                          (send add-friend-box show #f)]))]
                           [else (unless (false? make-noise)
                                   (play-sound (last sounds) #t))
                                 (let ((mbox (message-box "Blight - Invalid Tox ID"
                                                          "Sorry, that is an invalid Tox ID."
                                                          add-friend-error-dialog
                                                          (list 'ok 'stop))))
                                   (when (eq? mbox 'ok)
                                     (send add-friend-error-dialog show #f)))])))]))
#| ##################### END ADD FRIEND STUFF ####################### |#

; Preferences menu item for Edit
(define menu-preferences (new menu-item%
                              [parent menu-edit]
                              [label "Preferences"]
                              [shortcut #\R]
                              [help-string "Modify Blight preferences"]
                              [callback (λ (button event)
                                          (send preferences-box show #t))]))

; menu Help for menu bar
(define menu-help (new menu%
                       [parent frame-menu-bar]
                       [label "&Help"]
                       [help-string "Get information about Blight"]))

; About Blight menu item for Help
(define menu-help-about (new menu-item%
                             [parent menu-help]
                             [label "About Blight"]
                             [help-string "Show information about Blight"]
                             [callback (λ (button event)
                                         (send help-dialog show #t))]))

; send friend request
(define add-friend-button (new button%
                               [parent panel]
                               [label "Add friend"]
                               [callback (λ (button event)
                                           (send add-friend-box show #t))]))

; remove a friend
(define del-friend-dialog (new dialog%
                               [label "Remove a Tox friend"]
                               [style (list 'close-button)]))

; remove friend from list
(define delete-friend-button
  (new button%
       [parent panel]
       [label "Delete friend"]
       [callback (λ (button event)
                   (let ((friend-num (send list-box get-selection))
                         (mbox (message-box "Blight - Deleting Friend"
                                            "Are you sure you want to delete?"
                                            del-friend-dialog
                                            (list 'ok-cancel))))
                     (when (eq? mbox 'ok)
                       ; delete from tox friend list
                       (tox_del_friend my-tox friend-num)
                       ; save the blight data
                       (blight-save-data)
                       ; remove from list-box
                       (send list-box delete friend-num)
                       ; remove from list
                       (set! friend-list (delnode friend-list friend-num)))))]))

#| ############### START THE GUI, YO ############### |#
; show the frame by calling its show method
(send frame show #t)

#| ########### START CALLBACK PROCEDURE DEFINITIONS ########## |#
; set all the callback functions
(define on-friend-request
  (λ (mtox public-key data length userdata)
    ; convert public-key from bytes to string so we can display it
    (define id-hex (ptrtox->hextox public-key TOX_CLIENT_ID_SIZE))
    ; friend request dialog
    (define friend-request-dialog (new dialog%
                                       [label "Blight - Friend Request"]
                                       [style (list 'close-button)]))
    
    ; friend request text with modified text size
    (define friend-request-text (new text%
                                     [line-spacing 1.0]
                                     [auto-wrap #t]))
    (send friend-request-text change-style font-size-delta)
    
    ; canvas to print the friend request message
    (define friend-request-editor-canvas (new editor-canvas%
                                              [parent friend-request-dialog]
                                              [min-height 150]
                                              [min-width 600]
                                              [vert-margin 10]
                                              [editor friend-request-text]
                                              [style (list 'control-border 'no-hscroll
                                                           'auto-vscroll 'no-focus)]))
    
    ; panel to right-align our buttons
    (define friend-request-panel (new horizontal-panel%
                                      [parent friend-request-dialog]
                                      [alignment (list 'right 'center)]))
    
    (define ok (new button% [parent friend-request-panel]
                    [label "OK"]
                    [callback (λ (button event)
                                (send friend-request-dialog show #f)
                                ; add the friend
                                (tox_add_friend_norequest mtox public-key)
                                ; save the tox data
                                (blight-save-data)
                                ; play a sound because we accepted
                                (unless (false? make-noise)
                                  (play-sound (sixth sounds) #f))
                                ; append new friend to the gvector
                                (set! friend-list (append
                                                   friend-list
                                                   (list (new chat-window%
                                                              [this-label "a"]
                                                              [this-width 400]
                                                              [this-height 600]
                                                              [this-tox my-tox]))))
                                ; update friend list
                                (update-list-box)
                                ; add connection status icons to each friend
                                (do ((i 0 (+ i 1)))
                                  ((= i (tox_count_friendlist mtox)))
                                  (status-checker
                                   i
                                   (tox_get_friend_connection_status mtox i))))]))
    
    (define cancel (new button% [parent friend-request-panel]
                        [label "Cancel"]
                        [callback (λ (button event)
                                    ; close and reset the friend request dialog
                                    (send friend-request-dialog show #f)
                                    (send friend-request-text clear)
                                    (send friend-request-text change-style font-size-delta))]))
    (send friend-request-text insert (string-append
                                      id-hex
                                      "\nwould like to add you as a friend!\n"
                                      "Message: " data))
    (send friend-request-dialog show #t)))

(define on-friend-message
  (λ (mtox friendnumber message length userdata)
    (let* ((window (list-ref friend-list friendnumber))
           (editor (send window get-receive-editor))
           (name (send window get-name)))
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      (send editor insert
            (string-append name " [" (get-time) "]: " message "\n"))
      ; make a noise
      (unless (false? make-noise)
        (play-sound (first sounds) #t))
      ; add message to the history database
      (add-history my-id-hex (send window get-key) message 0))))

(define on-friend-name-change
  (λ (mtox friendnumber newname length userdata)
    (let ((window (list-ref friend-list friendnumber)))
      ; update the name in the list-box
      (send list-box set-string friendnumber newname)
      ; update the name in the gvector
      (send window set-name newname)
      ; update the name in the window
      (send window set-new-label (string-append "Blight - " newname))
      ; add connection status icon
      (status-checker friendnumber (tox_get_friend_connection_status mtox friendnumber)))))

(define on-status-type-change
  (λ (mtox friendnumber status userdata)
    (cond [(= status (_TOX_USERSTATUS-index 'NONE))
           ; if there is no special status, add a checkmark
           (send list-box set-string friendnumber
                 (string-append
                  "(✓) "
                  (send (list-ref friend-list friendnumber) get-name)))]
          ; if user is away, add a dash inside a circle
          [(= status (_TOX_USERSTATUS-index 'AWAY))
           (send list-box set-string friendnumber (string-append
                                                   "(⊖) "
                                                   (send
                                                    (list-ref friend-list friendnumber)
                                                    get-name)))]
          ; if user is busy, add an X inside a circle
          [(= status (_TOX_USERSTATUS-index 'BUSY))
           (send list-box set-string friendnumber (string-append
                                                   "(⊗) "
                                                   (send
                                                    (list-ref friend-list friendnumber)
                                                    get-name)))])))

(define on-connection-status-change
  (λ (mtox friendnumber status userdata)
    ; add a thingie that shows the friend is online
    (cond [(zero? status)
           ; if the user is offline, append his name with (X)
           (send list-box set-string friendnumber
                 (string-append
                  "(X) "
                  (send (list-ref friend-list friendnumber) get-name)))
           (unless (false? make-noise)
             (play-sound (third sounds) #t))]
          ; user is online, add a checkmark
          [else (send list-box set-string friendnumber
                      (string-append
                       "(✓) "
                       (send (list-ref friend-list friendnumber) get-name)))
                (unless (false? make-noise)
                  (play-sound (second sounds) #t))])))

; needs to be in its own thread, otherwise we'll d/c
; TODO:
; - check if filenumber is outside our range, which must mean
;   that there was an error, so we must adjust the filenumber accordingly
(define on-file-send-request
  (λ (mtox friendnumber filenumber filesize filename length userdata)
    (thread
     (λ ()
       (let ((mbox (message-box "Blight - File Send Request"
                                (string-append
                                 (send (list-ref friend-list friendnumber) get-name)
                                 " wants to send you "
                                 "\"" filename "\"")
                                #f
                                (list 'ok-cancel 'caution))))
         (cond [(eq? mbox 'ok)
                (let ((path (put-file "Select a file"
                                      #f
                                      download-path
                                      filename)))
                  (unless (false? path)
                    (define message-id (_TOX_FILECONTROL-index 'ACCEPT))
                    (define receive-editor
                      (send (list-ref friend-list friendnumber) get-receive-editor))
                    (tox_file_send_control mtox friendnumber 1 filenumber message-id #f 0)
                    (if (zero? filenumber)
                        ; our first receiving transfer, replace the null
                        (set! rtransfers (setnode rtransfers (open-output-file
                                                              path
                                                              #:mode 'binary
                                                              #:exists 'replace)
                                                  filenumber))
                        ; not our first, append to the list
                        (set! rtransfers (append rtransfers (list (open-output-file
                                                                   path
                                                                   #:mode 'binary
                                                                   #:exists 'replace)))))
                    (send receive-editor insert "\n***FILE TRANSFER HAS BEGUN***\n\n")))]))))))

; receive-send is 1 or 0
; 1 - sending
; 0 - receiving
(define on-file-control
  (λ (mtox friendnumber receive-send filenumber control-type data-ptr length userdata)
    (let* ((window (list-ref friend-list friendnumber))
           (receive-editor (send window get-receive-editor)))
      ; we've finished receiving the file
      (cond [(and (= control-type (_TOX_FILECONTROL-index 'FINISHED))
                  (zero? receive-send))
             (define data-bytes (make-sized-byte-string data-ptr length))
             (write-bytes data-bytes (list-ref rtransfers filenumber))
             ; close receive transfer
             (close-output-port (list-ref rtransfers filenumber))
             ; remove transfer from list
             (set! rtransfers (delnode rtransfers filenumber))
             ; notify user transfer has completed
             (send receive-editor insert "\n***FILE TRANSFER COMPLETED***\n\n")]
            ; cue that we're going to be sending the data now
            [(and (= control-type (_TOX_FILECONTROL-index 'ACCEPT))
                  (= receive-send 1))
             (send receive-editor insert "\n***FILE TRANSFER HAS BEGUN***\n\n")
             (send window send-data filenumber)]))))

(define on-file-data
  (λ (mtox friendnumber filenumber data-ptr length userdata)
    (define data-bytes (make-sized-byte-string data-ptr length))
    (write-bytes data-bytes (list-ref rtransfers filenumber))))

; register our callback functions
(tox_callback_friend_request my-tox on-friend-request #f)
(tox_callback_friend_message my-tox on-friend-message #f)
(tox_callback_name_change my-tox on-friend-name-change #f)
(tox_callback_user_status my-tox on-status-type-change #f)
(tox_callback_connection_status my-tox on-connection-status-change #f)
(tox_callback_file_send_request my-tox on-file-send-request #f)
(tox_callback_file_control my-tox on-file-control #f)
(tox_callback_file_data my-tox on-file-data #f)

; tox loop that only uses tox_do and sleeps for some amount of time
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       (tox_do my-tox)
       (sleep (/ (tox_do_interval my-tox) 1000))
       (loop)))))
