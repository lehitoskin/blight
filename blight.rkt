#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client
; most of these here are for the buddy-list
(require libtoxcore-racket ; wrapper
         "chat.rkt"         ; contains definitions for chat window
         "config.rkt"       ; default config file
         "callbacks.rkt"    ; inner procedure callback definitions
         "number-conversions.rkt" ; bin, dec, and hex conversions
         "helpers.rkt"      ; various useful functions
         ffi/unsafe         ; needed for neat pointer shenanigans
         json               ; for reading and writing to config file
         file/sha1          ; for hex-string->bytes
         db)                ; access sqlite db for stored history

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
along with this program. If not, see <http://www.gnu.org/licenses/>.")

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
; data-file is empty, use default settings
(cond [(zero? (file-size data-file))
       ; set username
       (tox_set_name my-tox my-name (string-length my-name))
       ; set status message
       (tox_set_status_message my-tox my-status-message (string-length
                                                         my-status-message))]
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
       (tox_load my-tox data-ptr size)])

; connect to DHT
(tox_bootstrap_from_address my-tox dht-address TOX_ENABLE_IPV6_DEFAULT dht-port
                            dht-public-key)

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
    ; necessary for saving the messenger
    (define size (tox_size my-tox))
    (define data-ptr (malloc 'atomic size))
    ; place all tox info into data-ptr
    (tox_save my-tox data-ptr)
    ; SAVE INFORMATION TO DATA
    ; dec->hex->bytes
    (let ((my-data #"")
          (data-port-out (open-output-file data-file
                                           #:mode 'binary
                                           #:exists 'truncate/replace)))
      (do ((i 0 (+ i 1)))
        ((= i size))
        (set! my-data
              (bytes-append my-data
                            (hex-string->bytes
                             (dec->hex (ptr-ref data-ptr _uint8_t i))))))
      (write-bytes my-data data-port-out)
      (close-output-port data-port-out))))

; obtain tox id
(define my-id-bytes (malloc 'atomic
                            (* TOX_FRIEND_ADDRESS_SIZE
                               (ctype-sizeof _uint8_t))))
(define my-id-hex "")
(tox_get_address my-tox my-id-bytes)
(do ((i 0 (+ i 1)))
  ((= i TOX_FRIEND_ADDRESS_SIZE))
  (set! my-id-hex
        (string-upcase
         (string-append my-id-hex
                        (dec->hex (ptr-ref my-id-bytes _uint8_t i))))))

#| ############ BEGIN DATABASE STUFF ################ |#
; DATABASE DATABASE! JUST LIVING IN THE DATABASE!
; WOWOW
(define sqlc
  (sqlite3-connect
   #:database db-file
   #:mode 'create))

; database initialization
; follows Venom's database scheme
(query-exec sqlc
            "create table if not exists History(
             id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
             userHash TEXT NOT NULL,
             contactHash TEXT NOT NULL,
             message TEXT NOT NULL,
             timestamp INTEGER NOT NULL,
             issent INTEGER NOT NULL);")

; index query
(query-exec sqlc
            "CREATE UNIQUE INDEX IF NOT EXISTS main_index
             ON History (userHash, contactHash, timestamp);")

; insert into history
;"INSERT INTO History (userHash, contactHash, message, timestamp, issent)
;VALUES ($USER, $CONTACT, $MESSAGE, $TIME, $SENDER);"

; get history
; maybe this will be useful for something like
; File -> Chat history -> Select user
; or even Right click user -> View Chat History
;"SELECT * FROM History WHERE userHash = $USER AND contactHash = $CONTACT
;AND timestamp > $OLDEST;"

; little procedure to wrap things up for us
(define clean-up
  (λ ()
    ; save tox info to data-file
    (blight-save-data)
    ; disconnect from the database
    (disconnect sqlc)
    ; close input port
    (close-input-port config-port-in)
    ; kill tox thread
    (kill-thread tox-loop-thread)
    ; this kills the tox
    (tox_kill my-tox)))

#| ############### BEGIN GUI STUFF ################## |#
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight"]
                   [width 400]
                   [height 300]
                   [x 0]
                   [y 0]))

; make a static text message in the frame
(define frame-msg (new message% [parent frame]
                       [label "Blight Buddy List"]
                       [min-width 40]))

(define username-frame-message (new message% [parent frame]
                                    [label my-name]
                                    [min-width
                                     (string-length my-name)]))
(send username-frame-message auto-resize #t)

(define status-frame-message (new message% [parent frame]
                                  [label my-status-message]
                                  [min-width
                                   (string-length my-status-message)]))
(send status-frame-message auto-resize #t)

#|
list-box [choices (list "Me")]
(send list-box set-data 0 "Me")
if talking to "Me" (list-box 0 maybe?), this is an echo window

(define list-size (tox_get_num_online_friends my-tox)
(tox_get_friendlist my-tox out-list list-size)
loop through out-list, populate list-box
  (send list-box append (ptr-ref out-list _uint8_t i)
                        (ptr-ref out-list_uint8_t i))
|#

; list box for friend list
(define list-box (new list-box%
                      [label "Select Buddy"]
                      [parent frame]
                      [min-height 250]
                      [style (list 'single 'vertical-label)]
                      [choices (list "Test")]
                      [callback (λ (l e)
                                  (when (eq? (send e get-event-type)
                                             'list-box-dclick)
                                    (send chat-frame set-label
                                          (send list-box get-data
                                                (first (send list-box get-selections))))
                                    (send chat-frame-msg set-label
                                          (send list-box get-data
                                                (first (send list-box get-selections))))
                                    (send chat-frame show #t)))]))

;(send list-box set (list "one" "two" "Three"))
; set data for each item in list-box
; data may be arbitrary, but a label will suffice
(send list-box set-data 0 "Test")
; obtain 0th friend's name, add it to list
#|(define friend-name-bytes (malloc 'atomic TOX_MAX_NAME_LENGTH))
(define friend-name-text "")
(printf "Getting friend name: ~a\n" (tox_get_name my-tox 0 friend-name-bytes))
(do ((i 0 (+ i 1)))
  ((= i TOX_MAX_NAME_LENGTH))
  (set! friend-name-text
        (string-append friend-name-text
                       (bytes->string/utf-8 
                        (ptr-ref friend-name-bytes _uint8_t i)))))
(send list-box append friend-name-text friend-name-text)|#

; panel for main frame
(define panel (new horizontal-panel%
                   [parent frame]))

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
(new button% [parent help-dialog]
     [label "&OK"]
     [callback (λ (button event)
                 (send help-dialog show #f))])

#| ############ MENU BAR AND STUFF ############## |#
; menu bar for the frame
(define frame-menu-bar (new menu-bar%
                            [parent frame]))

; menu File for menu bar
(define menu-file (new menu% [parent frame-menu-bar]
                       [label "&File"]
                       [help-string "Open, Quit, etc."]))

; Copy ID to Clipboard item for File
(new menu-item% [parent menu-file]
     [label "Copy ID to Clipboard"]
     [help-string "Copies your Tox ID to the clipboard"]
     [callback (λ (button event)
                 (send chat-clipboard set-clipboard-string
                       my-id-hex
                       (current-seconds)))])

; Quit menu item for File
; uses message-box with 'ok-cancel
(new menu-item% [parent menu-file]
     [label "&Quit"]
     [shortcut #\Q]
     [help-string "Quit Blight"]
     [callback (λ (button event)
                 ;(send exit-dialog show #t))])
                 (let ((mbox (message-box "Quit Blight"
                                          "Are you sure you want to quit Blight?"
                                          exit-dialog
                                          (list 'ok-cancel 'caution))))
                   (if (eq? mbox 'ok)
                       ((clean-up)
                        (exit))
                       null)))])

; menu Edit for menu bar
(define menu-edit (new menu% [parent frame-menu-bar]
                       [label "&Edit"]
                       [help-string "Modify Blight"]))

(define preferences-box (new dialog%
                             [label "Edit Preferences"]
                             [style (list 'close-button)]))

(define putfield (new text-field%
                      [parent preferences-box]
                      [label "New Username:"]
                      [style (list 'vertical-label
                                   'single)]
                      [callback (λ (l e)
                                  (when (eq? (send e get-event-type)
                                             'text-field-enter)
                                    ; set the new username
                                    (blight-save-config 'my-name-last (send l get-value))
                                    (send username-frame-message set-label
                                          (send l get-value))
                                    (tox_set_status_message my-tox (send l get-value)
                                                            (string-length (send l get-value)))
                                    (send l set-value "")))]))

(define pstfield (new text-field%
                      [parent preferences-box]
                      [label "New Status:"]
                      [style (list 'vertical-label
                                   'single)]
                      [callback (λ (l e)
                                  (when (eq? (send e get-event-type)
                                             'text-field-enter)
                                    ; set the new status
                                    (blight-save-config 'my-status-last (send l get-value))
                                    (send status-frame-message set-label
                                          (send l get-value))
                                    (tox_set_status_message my-tox (send l get-value)
                                                            (string-length (send l get-value)))
                                    (send l set-value "")))]))

; add a friend 'n' stuff
(define add-friend-box (new dialog%
                            [label "Add a new Tox friend"]
                            [style (list 'close-button)]))

; add friend with nickname
; TODO:
; check if friend nick is already in use
(define add-friend-nick-tfield (new text-field%
                                    [parent add-friend-box]
                                    [label "Friend name:"]
                                    [horiz-margin 38]))

; add friend with Tox ID
; TODO:
; DNS shit
(define add-friend-hex-tfield (new text-field%
                                   [parent add-friend-box]
                                   [label "Friend ID(X):"]
                                   [min-width 38]
                                   [callback (λ (l on-char)
                                               (if (tox-id? (send l get-value))
                                                   (send l set-label "Friend ID(✓):")
                                                   (send l set-label "Friend ID(X):")))]))

; message to send as a friend request
(define add-friend-message-tfield (new text-field%
                                       [parent add-friend-box]
                                       [label "Message:"]
                                       [min-width 38]
                                       [init-value "Please let me add you to my contact list"]))

; panel for the buttons
(define add-friend-panel (new horizontal-panel%
                              [parent add-friend-box]))

(define add-friend-error-dialog (new dialog%
                                     [label "Invalid Tox ID"]
                                     [style (list 'close-button)]))

; Preferences menu item for Edit
(new menu-item% [parent menu-edit]
     [label "Preferences"]
     [shortcut #\R]
     [help-string "Modify Blight preferences"]
     [callback (λ (button event)
                 (send preferences-box show #t))])

; OK button for preferences dialog box
(new button% [parent preferences-box]
     [label "OK"]
     [callback (λ (button event)
                 (send preferences-box show #f))])

; menu Help for menu bar
(define menu-help (new menu% [parent frame-menu-bar]
                       [label "&Help"]
                       [help-string "Get information about Blight"]))

; About Blight menu item for Help
(new menu-item% [parent menu-help]
     [label "About Blight"]
     [help-string "Show information about Blight"]
     [callback (λ (button event)
                 (send help-dialog show #t))])

; send friend request
(new button% [parent panel]
     [label "Add friend"]
     [callback (λ (button event)
                 (send add-friend-box show #t))])

; OK button for add-friend dialog box
(new button% [parent add-friend-panel]
     [label "OK"]
     [callback (λ (button event)
                 (let ((hex-tfield (send add-friend-hex-tfield get-value))
                       (nick-tfield (send add-friend-nick-tfield get-value))
                       (message-tfield (send add-friend-message-tfield get-value)))
                   ; add the friend to the friend list
                   (cond [(tox-id? hex-tfield)
                          ; convert hex to bytes
                          (define nick-bytes (malloc 'atomic
                                                     (* TOX_FRIEND_ADDRESS_SIZE
                                                        (ctype-sizeof _uint8_t))))
                          (do ((i 0 (+ i 1))
                               (j 0 (+ j 2)))
                            ((= i TOX_FRIEND_ADDRESS_SIZE))
                            (ptr-set! nick-bytes _uint8_t i
                                      (hex->dec
                                       (string-append
                                        (string (string-ref hex-tfield j))
                                        (string (string-ref hex-tfield (+ j 1)))))))
                          (let ((err (tox_add_friend my-tox
                                                     nick-bytes
                                                     message-tfield
                                                     (string-length message-tfield))))
                            (cond [(= err (_TOX_FAERR-index 'TOOLONG))
                                   "ERROR: TOX_FAERR_TOOLONG"]
                                  [(= err (_TOX_FAERR-index 'NOMESSAGE))
                                   "ERROR: TOX_FAERR_NOMESSAGE"]
                                  [(= err (_TOX_FAERR-index 'OWNKEY))
                                   "ERROR: TOX_FAERR_OWNKEY"]
                                  [(= err (_TOX_FAERR-index 'ALREADYSENT))
                                   "ERROR: TOX_FAERR_ALREADYSENT"]
                                  [(= err (_TOX_FAERR-index 'UNKNOWN))
                                   "ERROR: TOX_FAERR_UNKNOWN"]
                                  [(= err (_TOX_FAERR-index 'BADCHECKSUM))
                                   "ERROR: TOX_FAERR_BADCHECKSUM"]
                                  [(= err (_TOX_FAERR-index 'SETNEWNOSPAM))
                                   "ERROR: TOX_FAERR_SETNEWNOSPAM"]
                                  [(= err (_TOX_FAERR-index 'NOMEM))
                                   "ERROR: TOX_FAERR_NOMEM"]
                                  [else (displayln "All okay!")
                                        (send list-box append nick-tfield hex-tfield)
                                        (send add-friend-nick-tfield set-value "")
                                        (send add-friend-hex-tfield set-value "")
                                        (send add-friend-box show #f)]))]
                         [else (let ((mbox (message-box "Invalid Tox ID"
                                                        "Sorry, that is an invalid Tox ID."
                                                        add-friend-error-dialog
                                                        (list 'ok 'stop))))
                                 (when (eq? mbox 'ok)
                                   (send add-friend-error-dialog show #f)))])))])

; don't actually want to add a friend right now
(new button% [parent add-friend-panel]
     [label "Cancel"]
     [callback (λ (button event)
                 (send add-friend-nick-tfield set-value "")
                 (send add-friend-hex-tfield set-value "")
                 (send add-friend-box show #f))])

; TODO: Remove buddy from list
(new button% [parent panel]
     [label "Delete friend"])

#| ############### START THE GUI, YO ############### |#
; show the frame by calling its show method
(send frame show #t)

; tox loop that uses tox_wait
#|(define tox-wait-buffer (malloc 'atomic (tox_wait_data_size)))
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       (tox_wait_prepare my-tox tox-wait-buffer)
       (define err (tox_wait_execute tox-wait-buffer 1 999999)) ; timeout at 1.001 seconds
       (tox_wait_cleanup my-tox tox-wait-buffer)
       (cond [(= err 2) (tox_do my-tox)]
             [(= err 1) (tox_do my-tox)]
             [(zero? err) null])
       (loop)))))|#

; set all the callback functions
; procedure that returns my-tox because of ffi/unsafe shenanigans
(define grab-tox
  (λ ()
    my-tox))
; tox loop that only uses tox_do and sleeps for some amount of time
(define tox-loop-thread
  (thread
   (λ ()
     (let loop ()
       #|(tox_callback_name_change my-tox
                                 (on-friend-name-change (grab-tox)
                                                        friend-number
                                                        new-name
                                                        length)
                                 null)|#
       (tox_do my-tox)
       (sleep 1/2)
       (loop)))))
