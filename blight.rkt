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

(define running #t)

; instantiate Tox session
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))
; empty variables that get set! later
(define dht-address "")
(define dht-port 0)
(define dht-public-key "")
(define my-name "")
(define my-status-message "")

#| ############ BEGIN JSON STUFF ############ |#
; if blight-config.json does not exist, initalize
; it to default values, otherwise read from the
; file and set necessary values to what's inside

(define json-expression
  (hasheq 'dht-address dht-address-default
          'dht-port dht-port-default
          'dht-public-key dht-public-key-default
          'my-name-last my-name-default
          'my-status-last my-status-message-default))

; reusable procedure to save information to blight-config.json
(define blight-save-config
  (λ ()
    (let ((json
           (hash-set* json-expression
                      'my-name-last my-name
                      'my-status-last my-status-message))
          (config-port-out (open-output-file config-file
                                             #:mode 'text
                                             #:exists 'truncate/replace)))
      (json-null 'null)
      (write-json json config-port-out)
      (write-json "" config-port-out)
      (write-json (json-null) config-port-out)
      (close-output-port config-port-out))))

; blight-config.json is empty, set variables to defaults
(cond [(zero? (file-size config-file))
       ; set DHT information to default
       (set! dht-address dht-address-default)
       (set! dht-port dht-port-default)
       (set! dht-public-key dht-public-key-default)
       ; set name and status to default
       (set! my-name my-name-default)
       (set! my-status-message my-status-message-default)
       ; save info to newly created blight-config.json
       (blight-save-config)]
      ; blight-config.json contains values, read from them
      [(not (zero? (file-size config-file))) (let ((json-info (read-json config-port-in)))
                                               (set! dht-address (hash-ref json-info 'dht-address))
                                               (set! dht-port (hash-ref json-info 'dht-port))
                                               (set! dht-public-key (hash-ref json-info 'dht-public-key))
                                               (set! my-name (hash-ref json-info 'my-name-last))
                                               (set! my-status-message (hash-ref json-info 'my-status-last)))])

#| ############ BEGIN TOX STUFF ############ |#
; set username
; do not do this if data-file exists
(tox_set_name my-tox my-name (string-length my-name))

; set status message
; do not do this if data-file exists
(tox_set_status_message my-tox my-status-message (string-length
                                                  my-status-message))

; connect to DHT
(tox_bootstrap_from_address my-tox dht-address TOX_ENABLE_IPV6_DEFAULT dht-port
                            dht-public-key)

; necessary for saving and loading the messenger
(define size (tox_size my-tox))
(define data-ptr (malloc size))

; reusable procedure to save tox information to data-file
(define blight-save-data
  (λ ()
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

; if data-file is empty, initialize it
(unless (not (zero? (file-size data-file)))
  (blight-save-data))

; LOAD INFORMATION FROM DATA
;(tox_load my-tox data-ptr size)

; my Tox ID shenanigans
(define my-id-bytes (malloc (* TOX_FRIEND_ADDRESS_SIZE
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
    ; save information to blight-config.json
    (blight-save-config)
    ; save tox info to data-file
    (blight-save-data)
    ; this kills the tox
    (tox_kill my-tox)
    ; disconnect from the database
    (disconnect sqlc)
    ; close input ports
    (close-input-port config-port-in)
    (close-input-port data-port-in)))

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

; combo-field to choose online vs. all friends in the friend list?
; (define list-size (tox_get_num_online_friends my-tox)
; (tox_get_friendlist my-tox out-list list-size)

; list box for friend list
(define list-box (new list-box%
                      [label "Select Buddy"]
                      [parent frame]
                      [min-height 250]
                      [style (list 'single 'vertical-label)]
                      [choices (list "Me")]
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
; set data for each item in list-box
; data may be arbitrary, but a label will suffice
(send list-box set-data 0 "Me")
(send list-box append "test" "test")

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

; Add New Friend menu item for File
#|(new menu-item% [parent menu-file]
     [label "Add New Friend"]
     [shortcut #\N]
     [help-string "Add a new Tox friend"]
     [callback (λ (button event)
                     (send add-friend-box show #t))])|#

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
                       (and
                        (clean-up)
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
                                    (set! my-name (send l get-value))
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
                                    (set! my-status-message (send l get-value))
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

; Get Help menu item for Help
#|(new menu-item% [parent menu-help]
     [label "Get Help"]
     [shortcut #\H]
     [help-string "Have questions? Get help!"]
     [callback (λ (button event)
                 (send help-get-box show #t))])|#

; send friend request
(new button% [parent panel]
     [label "Add friend"]
     [callback (λ (button event)
                 (send add-friend-box show #t))])

; OK button for add-friend dialog box
(new button% [parent add-friend-panel]
     [label "OK"]
     [callback (λ (button event)
                 ; add the friend to the friend list
                 (cond [(tox-id? (send add-friend-hex-tfield get-value))
                        (send list-box append (send add-friend-nick-tfield get-value)
                              (send add-friend-nick-tfield get-value))
                        ;(tox_add_friend)
                        (send add-friend-nick-tfield set-value "")
                        (send add-friend-hex-tfield set-value "")
                        (send add-friend-box show #f)]
                       [else (let ((mbox (message-box "Invalid Tox ID"
                                                      "Sorry, that is an invalid Tox ID."
                                                      add-friend-error-dialog
                                                      (list 'ok 'stop))))
                               (when (eq? mbox 'ok)
                                 (send add-friend-error-dialog show #f)))]))])

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
