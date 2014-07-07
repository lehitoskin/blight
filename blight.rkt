#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client written in Racket
(require libtoxcore-racket ; wrapper
         "chat.rkt"         ; contains definitions for chat window
         "group.rkt"        ; contains definitions for group window
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

(define get-help-message
  "Need more help? Try adding leahtwoskin@toxme.se (or leahtwoskin@utox.org)
and bug the dev! Alternatively, you could join #tox-dev on freenode and see
if people have a similar problem.")

; instantiate Tox session
(define my-tox (tox-new TOX_ENABLE_IPV6_DEFAULT))

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
(define total-len 0)
(define sent 0)
(define percent 0)

; data-file is empty, use default settings
(cond [(zero? (file-size data-file))
       ; set username
       (set-name my-tox my-name)
       ; set status message
       (set-status-message my-tox my-status-message)]
      ; data-file is not empty, load from data-file
      [(not (zero? (file-size data-file)))
       ; load the messenger from data of size length
       (define size (file-size data-file))
       ; no conversions necessary because bytes-ref reports a decimal value
       (define my-bytes (file->bytes data-file #:mode 'binary))
       (printf "Loading from data file... ~a\n" (tox-load my-tox my-bytes size))])

; obtain our tox id
(define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
(get-address my-tox my-id-bytes)
(define my-id-hex (bytes->hex-string my-id-bytes TOX_FRIEND_ADDRESS_SIZE))

; connect to DHT
(display "Connecting to network... ")
(cond [(not (false? (bootstrap-from-address my-tox
                                            dht-address
                                            TOX_ENABLE_IPV6_DEFAULT
                                            dht-port
                                            dht-public-key)))
       (unless (false? make-noise)
         (play-sound (fourth sounds) #t))
       (displayln "Connected!")]
      [else (unless (false? make-noise)
              (play-sound (last sounds) #t))
            (displayln "Did not connect!")])

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
    (displayln "Saving data...")
    ; necessary for saving the messenger
    (define size (tox-size my-tox))
    (define data-bytes (make-bytes size))
    ; place all tox info into data-bytes
    (tox-save! my-tox data-bytes)
    ; SAVE INFORMATION TO DATA
    (let ((data-port-out (open-output-file data-file
                                           #:mode 'binary
                                           #:exists 'truncate/replace)))
      (write-bytes data-bytes data-port-out)
      (close-output-port data-port-out))
    (displayln "Done!")))

; little procedure to wrap things up for us
(define clean-up
  (λ ()
    ; save tox info to data-file
    ;(define data-thread (blight-save-data))
    (blight-save-data)
    ; disconnect from the database
    (disconnect sqlc)
    ; close config file input port
    (close-input-port config-port-in)
    ; kill tox thread
    (kill-thread tox-loop-thread)
    ; this kills the tox
    (tox-kill! my-tox)
    ; log out sound
    (unless (false? make-noise)
      (play-sound (fifth sounds) #f))
    ; make sure the data is completely saved
    #;(thread-wait data-thread)))

#| ############### BEGIN GUI STUFF ################## |#
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight - Friend List"]
                   [stretchable-width #t]
                   [height 300]))

; make a static text message in the frame
(define frame-msg (new message%
                       [parent frame]
                       [label "Blight Friend List"]))

(define username-frame-message (new message%
                                    [parent frame]
                                    [label my-name]))


(send username-frame-message auto-resize #t)

(define status-frame-message (new message%
                                  [parent frame]
                                  [label my-status-message]))

(send status-frame-message auto-resize #t)

; choices for status type changes
(define status-choice
  (new choice%
       [parent frame]
       [label ""]
       [stretchable-width #t]
       [choices '("Available"
                  "Away"
                  "Busy")]
       [selection (get-self-user-status my-tox)]
       [callback (λ (choice control-event)
                   (set-user-status my-tox (send choice get-selection)))]))

#| ################## BEGIN FRIEND LIST STUFF #################### |#

(define group-list (list (new group-window%
                              [this-label "Blight - Groupchat #0"]
                              [this-height 600]
                              [this-width 800]
                              [this-tox my-tox]
                              [group-number 0])))

; loop through and create as many chat-window%'s
; as there are friends and add them to the list
(define friend-list (for/list ([i (in-range (friendlist-length my-tox))])
                      (new chat-window%
                           [this-label "a"]
                           [this-width 400]
                           [this-height 600]
                           [this-tox my-tox])))

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
       [callback (λ (list-box control-event)
                   (match (send control-event get-event-type)
                     ['list-box-dclick
                      (define selection (send list-box get-selection))
                      (define window (if (< selection (length friend-list))
                                         (findf (λ (friend)
                                                  (string=? (send list-box get-data selection)
                                                            (send friend get-key friend)))
                                                friend-list)
                                         (findf (lambda (group)
                                                  (= (send group get-group-number) (- selection (length friend-list))))
                                                group-list)))
                      (send window show #t)]
                     [_ (void)]))]))

; set data for each item in list-box
; data may be arbitrary, but a label will suffice
(send list-box set-data 0 "0123456789ABCDEF")

; helper to avoid spamming notification sounds
(define status-checker
  (λ (friendnumber status)
    (let ((type (get-user-status my-tox friendnumber)))
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
    (define num-friends (friendlist-length my-tox))
    ; get number of groupchats
    (define num-groups (count-chatlist my-tox))
    (unless (zero? num-friends)
      (send list-box clear)
      (define friend-name-buf (make-bytes TOX_FRIEND_ADDRESS_SIZE))
      (define friend-key-buf (make-bytes TOX_CLIENT_ID_SIZE))
      ; loop until we get all our friends
      (do ((window-num 0 (+ window-num 1)))
        ((= window-num num-friends))
        (let* ((friend-name-length (get-name my-tox window-num friend-name-buf))
               (friend-name-text (bytes->string/utf-8
                                  (subbytes friend-name-buf 0 friend-name-length))))
          ; grab our friend's public key
          (get-client-id my-tox window-num friend-key-buf)
          (define friend-key-text (bytes->hex-string friend-key-buf TOX_CLIENT_ID_SIZE))
          (define friend-num (get-friend-number my-tox friend-key-buf))
          ; add to the friend list
          (send list-box append (string-append "(X) " friend-name-text) friend-key-text)
          ; make sure friend numbering is correct
          (send (list-ref friend-list window-num) set-friend-num friend-num)
          ; add to the list
          (send (list-ref friend-list window-num) set-name friend-name-text)
          (send (list-ref friend-list window-num) set-key friend-key-text)
          (send (list-ref friend-list window-num) set-new-label
                (string-append "Blight - " friend-name-text))
          ; update our friends' status icon
          (status-checker window-num (get-friend-connection-status my-tox window-num)))))
    ; add the current groupchats to the bottom of the list
    (unless (zero? num-groups)
      (do ((i 0 (+ i 1)))
        ((= i num-groups))
        (send list-box append (format "Group Chat #~a" i) i)))))
(update-list-box)

; panel for choice and buttons
(define panel (new horizontal-panel%
                   [parent frame]
                   [stretchable-height #f]
                   [alignment (list 'right 'center)]))
#| ################## END FRIEND LIST STUFF #################### |#

#| ################### BEGIN MENU BAR STUFF #################### |#
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

; dialog box when exiting
(define exit-dialog (new dialog%
                         [label "Exit Blight"]
                         [style (list 'close-button)]))

; Quit menu item for File
; uses message-box with 'ok-cancel
(define menu-quit
  (new menu-item%
       [parent menu-file]
       [label "&Quit"]
       [shortcut #\Q]
       [help-string "Quit Blight"]
       [callback (λ (button event)
                   (let ((mbox (message-box/custom
                                "Blight - Quit Blight"
                                "Are you sure you want to quit Blight?"
                                "&OK"
                                "&Cancel"
                                #f
                                exit-dialog
                                (list 'caution 'no-default))))
                     (cond [(= mbox 1) (clean-up) (exit)])))]))

; menu Edit for menu bar
(define menu-edit (new menu%
                       [parent frame-menu-bar]
                       [label "&Edit"]
                       [help-string "Modify Blight"]))

; Preferences menu item for Edit
(define menu-preferences (new menu-item%
                              [parent menu-edit]
                              [label "Preferences"]
                              [shortcut #\R]
                              [help-string "Modify Blight preferences"]
                              [callback (λ (button event)
                                          (send preferences-box show #t))]))

(define help-get-dialog (new dialog%
                             [label "Blight - Get Help"]
                             [style (list 'close-button)]))

(define help-get-text (new text%
                           [line-spacing 1.0]
                           [auto-wrap #t]))
(send help-get-text change-style black-style)
(send help-get-text insert get-help-message)

(define help-get-editor-canvas
  (new editor-canvas%
       [parent help-get-dialog]
       [min-height 100]
       [min-width 600]
       [vert-margin 10]
       [editor help-get-text]
       [style (list 'control-border 'no-hscroll
                    'auto-vscroll 'no-focus)]))

(define help-get-ok
  (new button%
       [parent help-get-dialog]
       [label "&OK"]
       [callback (λ (button event)
                   (send help-get-dialog show #f))]))

; dialog box when looking at Help -> About
(define help-about-dialog (new dialog%
                               [label "Blight - License"]
                               [style (list 'close-button)]))

(define help-about-text (new text%
                             [line-spacing 1.0]
                             [auto-wrap #t]))
(send help-about-text change-style black-style)
(send help-about-text insert license-message)

; canvas to print the license message
(define help-about-editor-canvas
  (new editor-canvas%
       [parent help-about-dialog]
       [min-height 380]
       [min-width 600]
       [vert-margin 10]
       [editor help-about-text]
       [style (list 'control-border 'no-hscroll
                    'auto-vscroll 'no-focus)]))

; button to close the About Blight window
(define help-about-ok
  (new button%
       [parent help-about-dialog]
       [label "&OK"]
       [callback (λ (button event)
                   (send help-about-dialog show #f))]))

; menu Help for menu bar
(define menu-help (new menu%
                       [parent frame-menu-bar]
                       [label "&Help"]
                       [help-string "Get information about Blight"]))

; About Blight menu item for Help
(define menu-help-get-help (new menu-item%
                                [parent menu-help]
                                [label "Get Help"]
                                [help-string "Get Help with Blight"]
                                [callback (λ (button event)
                                            (send help-get-dialog show #t))]))

; About Blight menu item for Help
(define menu-help-about (new menu-item%
                             [parent menu-help]
                             [label "About Blight"]
                             [help-string "Show information about Blight"]
                             [callback (λ (button event)
                                         (send help-about-dialog show #t))]))
#| #################### END MENU BAR STUFF ################## |#

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
                                  (when (eq? (send e get-event-type)
                                             'text-field-enter)
                                    (let ((username (send l get-value)))
                                      ; refuse to set the status if it's empty
                                      (unless (string=? username "")
                                        ; set the new username
                                        (blight-save-config 'my-name-last username)
                                        (send username-frame-message set-label username)
                                        (set-name my-tox username)
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
                       (set-name my-tox username)
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
                                        (set-status-message my-tox status)
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
                       (set-status-message my-tox status)
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
                       (set-nospam! my-tox
                                    ; largest (random) can accept
                                    ; corresponds to "FFFFFF2F"
                                    (random 4294967087))
                       ; save our changes
                       (blight-save-data)
                       ; set new tox id
                       (get-address my-tox my-id-bytes)
                       (set! my-id-hex
                             (bytes->hex-string my-id-bytes TOX_FRIEND_ADDRESS_SIZE)))))]))

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
                             ; the hex field is empty, nick field cannot be empty
                             (and (string=? hex-tfield "")
                                  (and (not (string=? nick-tfield ""))
                                       ; make sure we get a response from the DNS
                                       (not (false? (tox-dns1 nick-tfield domain)))))
                             ; the nick field is empty, hex field cannot be empty
                             (and (string=? nick-tfield "")
                                  ; make sure hex field is a proper tox id
                                  (tox-id? hex-tfield)))
                            ; convert hex to bytes
                            (define nick-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
                            ; we're doing a direct friend add
                            (cond [(string=? nick-tfield "")
                                   ; obtain the byte form of the id
                                   (set! nick-bytes
                                         (hex-string->bytes
                                          hex-tfield
                                          TOX_FRIEND_ADDRESS_SIZE))]
                                  ; we're doing a dns lookup
                                  [(string=? hex-tfield "")
                                   ; obtain the id from the dns query
                                   (define friend-hex (tox-dns1 nick-tfield domain))
                                   ; obtain the byte form of the id
                                   (set! nick-bytes
                                         (hex-string->bytes
                                          friend-hex
                                          TOX_FRIEND_ADDRESS_SIZE))])
                            (let ((err (add-friend my-tox
                                                   nick-bytes
                                                   message-tfield)))
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
                                          ; append new friend to the list
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
                                          (send add-friend-box show #f)
                                          ; the invite list needs to be updated for
                                          ; the groupchat windows that still exist
                                          (unless (zero? (length group-list))
                                            (do ((i 0 (+ i 1)))
                                              ((= i (count-chatlist my-tox)))
                                              (send
                                               (list-ref group-list i)
                                               update-invite-list)))]))]
                           ; something went wrong!
                           [else (unless (false? make-noise)
                                   (play-sound (last sounds) #t))
                                 (let ((mbox (message-box
                                              "Blight - Invalid Tox ID"
                                              "Sorry, that is an invalid Tox ID or DNS nick."
                                              add-friend-error-dialog
                                              (list 'ok 'stop))))
                                   (when (eq? mbox 'ok)
                                     (send add-friend-error-dialog show #f)))])))]))
#| ##################### END ADD FRIEND STUFF ####################### |#

#| ####################### BEGIN GROUP STUFF ######################## |#
(define add-group-button
  (new button%
       [parent panel]
       [label "Add group"]
       [callback (λ (button event)
                   ; we're out of groupchat windows!
                   ; spawn a new one
                   (cond [(zero? (length group-list))
                          (set! group-list
                                (setnode group-list
                                         (new group-window%
                                              [this-label "Blight - Groupchat #0"]
                                              [this-height 600]
                                              [this-width 800]
                                              [this-tox my-tox]
                                              [group-number 0]) 0))])
                   (let ((err (add-groupchat my-tox)))
                     (cond
                       ; there's more than one groupchat
                       [(> err 0)
                        (set! group-list
                              (append
                               group-list
                               (list
                                (new group-window%
                                     [this-label (format "Blight - Groupchat #~a" err)]
                                     [this-height 600]
                                     [this-width 800]
                                     [this-tox my-tox]
                                     [group-number err]))))
                        (update-list-box)]
                       ; we're calling the 0th groupchat window
                       [(and (zero? err)
                             (= (length group-list) 1))
                        (update-list-box)])))]))

(define del-group-button
  (new button%
       [parent panel]
       [label "Del group"]
       [callback (λ (button event)
                   (let ((num (send list-box get-selection)))
                     ; unless num is a friend and we have no groups right now
                     (unless (and (<= num (length friend-list))
                                  (zero? (count-chatlist my-tox)))
                       ; delete from tox groupchat list
                       (del-groupchat! my-tox (- num (length friend-list)))
                       ; remove from list-box
                       (send list-box delete num)
                       ; remove from list
                       (set! group-list (delnode group-list (- num (length friend-list)))))))]))
#| ####################### END GROUP STUFF ########################## |#

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
       [label "Del friend"]
       [callback (λ (button event)
                   (let ((friend-num (send list-box get-selection))
                         (mbox (message-box "Blight - Deleting Friend"
                                            "Are you sure you want to delete?"
                                            del-friend-dialog
                                            (list 'ok-cancel))))
                     (when (eq? mbox 'ok)
                       ; delete from tox friend list
                       (del-friend! my-tox friend-num)
                       ; save the blight data
                       (blight-save-data)
                       ; remove from list-box
                       (send list-box delete friend-num)
                       ; remove from list
                       (set! friend-list (delnode friend-list friend-num))
                       ; the invite list needs to be updated for
                       ; the groupchat windows that still exist
                       (unless (zero? (length group-list))
                         (do ((i 0 (+ i 1)))
                           ((= i (count-chatlist my-tox)))
                           (send (list-ref group-list i) update-invite-list))))))]))

#| ############### START THE GUI, YO ############### |#
; show the frame by calling its show method
(send frame show #t)

#| ########### START CALLBACK PROCEDURE DEFINITIONS ########## |#
; set all the callback functions
(define on-friend-request
  (λ (mtox public-key data len userdata)
    ; convert public-key from bytes to string so we can display it
    (define id-hex (bytes->hex-string public-key TOX_CLIENT_ID_SIZE))
    ; friend request dialog
    (define friend-request-dialog (new dialog%
                                       [label "Blight - Friend Request"]
                                       [style (list 'close-button)]))
    
    ; friend request text with modified text size
    (define friend-request-text (new text%
                                     [line-spacing 1.0]
                                     [auto-wrap #t]))
    (send friend-request-text change-style black-style)
    
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
                                (add-friend-norequest mtox public-key)
                                ; save the tox data
                                (blight-save-data)
                                ; play a sound because we accepted
                                (unless (false? make-noise)
                                  (play-sound (sixth sounds) #f))
                                ; append new friend to the list
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
                                  ((= i (friendlist-length mtox)))
                                  (status-checker
                                   i
                                   (get-friend-connection-status mtox i)))
                                ; the invite list needs to be updated for
                                ; the groupchat windows that still exist
                                (unless (zero? (length group-list))
                                  (do ((i 0 (+ i 1)))
                                    ((= i (count-chatlist my-tox)))
                                    (send (list-ref group-list i) update-invite-list))))]))
    
    (define cancel (new button% [parent friend-request-panel]
                        [label "Cancel"]
                        [callback (λ (button event)
                                    ; close and reset the friend request dialog
                                    (send friend-request-dialog show #f)
                                    (send friend-request-text clear)
                                    (send friend-request-text change-style black-style))]))
    (send friend-request-text insert (string-append
                                      id-hex
                                      "\nwould like to add you as a friend!\n"
                                      "Message: " data))
    (send friend-request-dialog show #t)))

(define on-friend-message
  (λ (mtox friendnumber message len userdata)
    (let* ((window (list-ref friend-list friendnumber))
           (editor (send window get-receive-editor))
           (name (send window get-name)))
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      ; if the current cursor position is not at the end, move there
      (cond [(not (= (send editor get-start-position)
                     (send editor get-end-position)))
             (send editor move-position 'end)
             (send editor insert
                   (string-append "[" (get-time) "] " name ": "))
             ; implying
             (if (string=? (substring message 0 1) ">")
                 (imply editor message)
                 (send editor insert (string-append message "\n")))]
            ; otherwise just insert the message
            [(= (send editor get-start-position)
                (send editor get-end-position))
             (send editor insert
                   (string-append "[" (get-time) "] " name ": "))
             ; implying
             (if (string=? (substring message 0 1) ">")
                 (imply editor message)
                 (send editor insert (string-append message "\n")))])
      ; make a noise
      (unless (false? make-noise)
        (play-sound (first sounds) #t))
      ; add message to the history database
      (add-history my-id-hex (send window get-key) message 0))))

(define on-friend-action
  (λ (mtox friendnumber action len userdata)
    (let* ((window (list-ref friend-list friendnumber))
           (editor (send window get-receive-editor))
           (name (send window get-name)))
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      ; if the current cursor position is not at the end, move there
      (cond [(not (= (send editor get-start-position)
                     (send editor get-end-position)))
             (send editor move-position 'end)
             (send editor insert
                   (string-append "** [" (get-time) "] " name " " action "\n"))]
            ; otherwise just insert the message
            [(= (send editor get-start-position)
                (send editor get-end-position))
             (send editor insert
                   (string-append "** [" (get-time) "] " name " " action "\n"))])
      ; make a noise
      (unless (false? make-noise)
        (play-sound (first sounds) #t))
      ; add message to the history database
      (add-history my-id-hex (send window get-key) (string-append "ACTION: " action) 0))))

(define on-friend-name-change
  (λ (mtox friendnumber newname len userdata)
    (let ((window (list-ref friend-list friendnumber)))
      ; update the name in the list-box
      (send list-box set-string friendnumber newname)
      ; update the name in the list
      (send window set-name newname)
      ; update the name in the window
      (send window set-new-label (string-append "Blight - " newname))
      ; add connection status icon
      (status-checker friendnumber (get-friend-connection-status mtox friendnumber)))))

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

; needs to be in its own thread, otherwise we'll d/c(?)
(define on-file-send-request
  (λ (mtox friendnumber filenumber filesize filename len userdata)
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
                    (send-file-control mtox friendnumber #t filenumber message-id #f 0)
                    (set! sent 0)
                    (set! total-len filesize)
                    (set! percent 0)
                    (send (list-ref friend-list friendnumber) set-gauge-pos percent)
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
                    ; if the current cursor position is not at the end, move there
                    (cond [(not (= (send receive-editor get-start-position)
                                   (send receive-editor get-end-position)))
                           (send receive-editor move-position 'end)
                           (send receive-editor insert "\n***FILE TRANSFER HAS BEGUN***\n\n")]
                          ; otherwise just insert the message
                          [(= (send receive-editor get-start-position)
                              (send receive-editor get-end-position))
                           (send receive-editor insert
                                 "\n***FILE TRANSFER HAS BEGUN***\n\n")])))]))))))

(define on-file-control
  (λ (mtox friendnumber sending? filenumber control-type data-ptr len userdata)
    (let* ((window (list-ref friend-list friendnumber))
           (receive-editor (send window get-receive-editor)))
      ; we've finished receiving the file
      (cond [(and (= control-type (_TOX_FILECONTROL-index 'FINISHED))
                  (false? sending?))
             (define data-bytes (make-sized-byte-string data-ptr len))
             (write-bytes data-bytes (list-ref rtransfers filenumber))
             ; close receive transfer
             (close-output-port (list-ref rtransfers filenumber))
             ; remove transfer from list
             (set! rtransfers (delnode rtransfers filenumber))
             ; notify user transfer has completed
             ; if the current cursor position is not at the end, move there
             (cond [(not (= (send receive-editor get-start-position)
                            (send receive-editor get-end-position)))
                    (send receive-editor move-position 'end)
                    (send receive-editor insert "\n***FILE TRANSFER COMPLETED***\n\n")]
                   ; otherwise just insert the message
                   [(= (send receive-editor get-start-position)
                       (send receive-editor get-end-position))
                    (send receive-editor insert
                          "\n***FILE TRANSFER COMPLETED***\n\n")])]
            ; cue that we're going to be sending the data now
            [(and (= control-type (_TOX_FILECONTROL-index 'ACCEPT))
                  (not (false? sending?)))
             ; if the current cursor position is not at the end, move there
             (cond [(not (= (send receive-editor get-start-position)
                            (send receive-editor get-end-position)))
                    (send receive-editor move-position 'end)
                    (send receive-editor insert "\n***FILE TRANSFER HAS BEGUN***\n\n")]
                   ; otherwise just insert the message
                   [(= (send receive-editor get-start-position)
                       (send receive-editor get-end-position))
                    (send receive-editor insert
                          "\n***FILE TRANSFER HAS BEGUN***\n\n")])
             (send window send-data filenumber)]))))

(define on-file-data
  (λ (mtox friendnumber filenumber data-ptr len userdata)
    (define data-bytes (make-sized-byte-string data-ptr len))
    (write-bytes data-bytes (list-ref rtransfers filenumber))
    (set! sent (+ sent len))
    (set! percent (fl->exact-integer (truncate (* (exact->inexact (/ sent total-len)) 100))))
    (send (list-ref friend-list friendnumber) set-gauge-pos percent)))

(define on-group-invite
  (λ (mtox friendnumber group-public-key userdata)
    (let* ((friendname (send (list-ref friend-list friendnumber) get-name))
           (mbox (message-box "Blight - Groupchat Invite"
                              (string-append friendname
                                             " has invited you to a groupchat!")
                              #f
                              (list 'ok-cancel 'caution))))
      (when (eq? mbox 'ok)
        ; we're out of groupchat windows!
        ; spawn a new one
        (cond [(zero? (length group-list))
               (set! group-list
                     (setnode group-list
                              (new group-window%
                                   [this-label "Blight - Groupchat #0"]
                                   [this-height 600]
                                   [this-width 800]
                                   [this-tox my-tox]
                                   [group-number 0]) 0))])
        (define err (join-groupchat mtox friendnumber group-public-key))
        (cond [(> err 0)
               (set! group-list
                     (append (list (new group-window%
                                        [this-label (format "Blight - Groupchat #~a" err)]
                                        [this-height 600]
                                        [this-width 800]
                                        [this-tox my-tox]
                                        [group-number err]))))
               (update-list-box)]
              ; we're calling the 0th groupchat window
              [(and (zero? err)
                    (= (length group-list) 1))
               (update-list-box)])))))

(define on-group-message
  (λ (mtox groupnumber friendgroupnumber message len userdata)
    (let* ((window (list-ref group-list groupnumber))
           (editor (send window get-receive-editor))
           (name-buf (make-bytes TOX_MAX_NAME_LENGTH))
           (len (get-group-peername! mtox groupnumber friendgroupnumber name-buf))
           (name (bytes->string/utf-8 (subbytes name-buf 0 len))))
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      ; if the current cursor position is not at the end, move there
      (cond [(not (= (send editor get-start-position)
                     (send editor get-end-position)))
             (send editor move-position 'end)
             (send editor insert
                   (string-append "[" (get-time) "] " name ": "))
             ; implying
             (if (and (not (zero? (string-length message)))
                      (string=? (substring message 0 1) ">"))
                 (imply editor message)
                 (send editor insert (string-append message "\n")))]
            ; otherwise just insert the message
            [(= (send editor get-start-position)
                (send editor get-end-position))
             (send editor insert
                   (string-append "[" (get-time) "] " name ": "))
             ; implying
             (if (and (not (zero? (string-length message)))
                      (string=? (substring message 0 1) ">"))
                 (imply editor message)
                 (send editor insert (string-append message "\n")))]))))

(define on-group-action
  (λ (mtox groupnumber friendgroupnumber action len userdata)
    (let* ((window (list-ref group-list groupnumber))
           (editor (send window get-receive-editor))
           (name-buf (make-bytes TOX_MAX_NAME_LENGTH))
           (len (get-group-peername! mtox groupnumber friendgroupnumber name-buf))
           (name (bytes->string/utf-8 (subbytes name-buf 0 len))))
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      ; if the current cursor position is not at the end, move there
      (cond [(not (= (send editor get-start-position)
                     (send editor get-end-position)))
             (send editor move-position 'end)
             (send editor insert
                   (string-append "** [" (get-time) "] " name " " action "\n"))]
            ; otherwise just insert the message
            [(= (send editor get-start-position)
                (send editor get-end-position))
             (send editor insert
                   (string-append "** [" (get-time) "] " name " " action "\n"))]))))

(define on-group-namelist-change
  (λ (mtox groupnumber peernumber change userdata)
    (let ((lbox (send (list-ref group-list groupnumber) get-list-box)))
      (cond [(= change (_TOX_CHAT_CHANGE_PEER-index 'ADD))
             (define name-buf (make-bytes TOX_MAX_NAME_LENGTH))
             (define len (get-group-peername! mtox groupnumber peernumber name-buf))
             (define name (bytes->string/utf-8 (subbytes name-buf 0 len)))
             (send lbox append name)
             (send lbox set-label
                   (format "~a Peers" (get-group-number-peers mtox groupnumber)))]
            [(= change (_TOX_CHAT_CHANGE_PEER-index 'DEL))
             (send lbox delete peernumber)
             (send lbox set-label
                   (format "~a Peers" (get-group-number-peers mtox groupnumber)))]
            [(= change (_TOX_CHAT_CHANGE_PEER-index 'NAME))
             (define name-buf (make-bytes TOX_MAX_NAME_LENGTH))
             (define len (get-group-peername! mtox groupnumber peernumber name-buf))
             (define name (bytes->string/utf-8 (subbytes name-buf 0 len)))
             (send lbox set-string peernumber name)]))))

; register our callback functions
(callback-friend-request my-tox on-friend-request)
(callback-friend-message my-tox on-friend-message)
(callback-friend-action my-tox on-friend-action)
(callback-name-change my-tox on-friend-name-change)
(callback-user-status my-tox on-status-type-change)
(callback-connection-status my-tox on-connection-status-change)
(callback-file-send-request my-tox on-file-send-request)
(callback-file-control my-tox on-file-control)
(callback-file-data my-tox on-file-data)
(callback-group-invite my-tox on-group-invite)
(callback-group-message my-tox on-group-message)
(callback-group-action my-tox on-group-action)
(callback-group-namelist-change my-tox on-group-namelist-change)

; tox loop that only uses tox_do and sleeps for some amount of time
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       (tox-do my-tox)
       (sleep (/ (tox-do-interval my-tox) 1000))
       (loop)))))
