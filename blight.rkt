#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client
(require libtoxcore-racket ; wrapper
         db                 ; access db for stored info
         file/sha1)         ; hex-string procedures

; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight"]
                   [width 400]
                   [height 600]))

; make a static text message in the frame
(define frame-msg (new message% [parent frame]
                       [label "Blight"]))

#| ########## CANVAS AND OTHER FIELD DEFINITIONS ########## |#
; an editor canvas where messages will appear(?)
(define editor-canvas (new editor-canvas%
                           [parent frame]
                           [style (list 'control-border 'no-hscroll
                                        'auto-vscroll)]
                           [min-height 400]
                           [vert-margin 10]
                           [enabled #t]))

; key event when the user presses Enter
(define enter (new key-event%
                   [key-code #\return]))

; not sure what good this is. text-field% sends to text% I guess?
(define text (new text%
                  [line-spacing 1.0]))

; create a text-field to enter a message
(define tfield (new text-field%
                    [label "Change title:"]
                    [parent frame]
                    [vert-margin 50]
                    [enabled #t]
                    [callback (λ (on-char enter)
                                (send text get-text))]))

; dialog box when exiting
(define exit-dialog (new dialog%
                         [label "Exit Blight"]
                         [style (list 'close-button)]))

; panel for main frame
(define panel (new horizontal-panel%
                   [parent frame]))

; panel for exit-dialog
(define exit-panel (new horizontal-panel%
                        [parent exit-dialog]))

; message for the exit-panel
(define exit-panel-msg (new message%
                            [parent exit-panel]
                            [label "Are you sure you want to quit?"]
                            [vert-margin 50]))

; pane for buttons for exit-dialog so we get the buttons
; underneath the message (?) doesn't seem to want to work
#|(define exit-pane (new horizontal-pane%
                       [parent exit-panel]
                       [vert-margin 100]))|#

#| ############ MENU BAR AND STUFF ############## |#
; menu bar for the frame
(define frame-menu-bar (new menu-bar%
                            [parent frame]))

; menu File for menu bar
(define menu-file (new menu% [parent frame-menu-bar]
                       [label "File"]
                       [help-string "Open, Quit, etc."]))

; Status menu item for File
; changes both message and availability
#|(new menu-item% [parent menu-file]
     [label "Change Status"]
     [help-string "Change status message/availability"]
     [callback (λ (button event)
                 (send status-box show #t))])|#

; Open menu item for File
#|(new menu-item% [parent menu-file]
     [label "Open"]
     [shortcut #\O]
     [help-string "Open log"]
     [callback (λ (button event)
                     (send open-file-box show #t))])|#

; Quit menu item for File
(new menu-item% [parent menu-file]
     [label "Quit"]
     [shortcut #\Q]
     [help-string "Quit Blight"]
     [callback (λ (button event)
                 (send exit-dialog show #t))])

; menu Edit for menu bar
(define menu-edit (new menu% [parent frame-menu-bar]
                      [label "Edit"]
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
                       [label "Help"]
                       [help-string "Get information about Blight"]))

; About Blight menu item for Help
#|(new menu-item% [parent menu-help]
     [label "About Blight"]
     [help-string "Show information about Blight"]
     [callback (λ (button event)
                 (send help-blight-box show #t))])|#

; Get Help menu item for Help
#|(new menu-item% [parent menu-help]
     [label "Get Help"]
     [shortcut #\H]
     [help-string "Have questions? Get help!"]
     [callback (λ (button event)
                 (send help-get-box show #t))])|#

#| ########### BUTTONS AND STUFF ################# |#
; panel button for sending the message from tfield
(new button% [parent panel]
     [label "Send"]
     [callback (λ (button event)
                 (and
                  ; set new title to what's inside tfield
                  (send frame-msg set-label (send tfield get-value))
                  ; reset tfield to empty
                  (send tfield set-value "")))])

; exit-dialog button - Yes, I am sure I want to close
(new button% [parent exit-panel]
     [label "Yes"]
     ; confirm exit
     [callback (λ (button event)
                 (exit))])

; exit-dialog button - No I am not sure I want to close
(new button% [parent exit-panel]
     [label "No"]
     ; close the dialog box
     [callback (λ (button event)
                 (send exit-dialog show #f))])

#| ############### START THE GUI, YO ############### |#
; show the frame by calling its show method
(send frame show #t)

#| ############ BEGIN TOX STUFF ############ |#
#|(define my-name "Blight Tester")
(define my-status-message "Toxing on Blight")
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))

(tox_set_status_message my-tox my-status-message (string-length
                                                  my-status-message))

; connect to DHT
(define dht-address "192.254.75.98")
(define dht-port 33445)
(define dht-public-key
  (hex-string->bytes
   "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074"))

(tox_bootstrap_from_address my-tox dht-address TOX_ENABLE_IPV6_DEFAULT dht-port
                            dht-public-key)

(tox_kill my-tox)|#