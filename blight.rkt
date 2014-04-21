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
         ffi/unsafe         ; needed for neat pointer shenanigans
         db)                ; access db for stored info

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

#| ############ BEGIN TOX STUFF ############ |#
; instantiate Tox session
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))
; necessary for saving and loading the messenger
;(define len (tox_size my-tox))
;(define data-ptr (malloc len))

; LOAD SAVED INFORMATION FROM DB

;(tox_load my-tox)

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

; set status message
(tox_set_status_message my-tox my-status-message (string-length
                                                  my-status-message))

; connect to DHT
(tox_bootstrap_from_address my-tox dht-address TOX_ENABLE_IPV6_DEFAULT dht-port
                            dht-public-key)

; save messenger
;(displayln "Saving Tox.")
;(tox_save my-tox data-ptr)

; load messenger
;(displayln "Loading Tox. 0 means success. -1 means failure")
;(tox_load my-tox data-ptr len)

#| ############ BEGIN DATABASE STUFF ################ |#
; DATABASE DATABASE! JUST LIVING IN THE DATABASE!
; WOWOW
(define sqlc
  (sqlite3-connect
   #:database db-path
   #:mode 'create))

; database initialization
(query-exec sqlc
            "create table if not exists blightuser
             (blightuserkey INTEGER PRIMARY KEY,
                username TEXT,statusmessage TEXT);")

; little procedure to wrap things up for us
(define clean-up
  (λ ()
    ; save tox information
    ; tox_save + save to database
    ; this kills the tox
    (tox_kill my-tox)
    ; disconnect from the database
    (disconnect sqlc)))

#| ############### BEGIN GUI STUFF ################## |#
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight"]
                   [width 400]
                   [height 300]))

; make a static text message in the frame
(define frame-msg (new message% [parent frame]
                       [label "Blight Buddy List"]
                       [min-width 40]))

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
(send list-box append "al" "al")
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

; Status menu item for File
; changes both message and availability
#|(new menu-item% [parent menu-file]
     [label "Change Status"]
     [help-string "Change status message/availability"]
     [callback (λ (button event)
                 (send status-box show #t))])|#

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
                                          (list 'ok-cancel))))
                   (if (eq? mbox 'ok)
                       (and
                        (clean-up)
                        (exit))
                       null)))])

; menu Edit for menu bar
(define menu-edit (new menu% [parent frame-menu-bar]
                       [label "&Edit"]
                       [help-string "Modify Blight"]))

; Preferences menu item for Edit
#|(new menu-item% [parent menu-edit]
     [label "Preferences"]
     [shortcut #\R]
     [help-string "Modify Blight preferences"]
     [callback (λ (button event)
                 (send preferences-box show #t))])|#

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

(new button% [parent panel]
     [label "Add friend"])

(new button% [parent panel]
     [label "Delete friend"])

#| ############### START THE GUI, YO ############### |#
; show the frame by calling its show method
(send frame show #t)
