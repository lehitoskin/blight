#!/usr/bin/env racket
#lang racket/gui
; blight.rkt
; GUI Tox client
(require libtoxcore-racket ; wrapper
         db                 ; access db for stored info
         file/sha1)         ; hex-string procedures

(define license-message
  " Blight - a Tox client written in Racket.
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

#|
 # two ways (so far) to go about sending a message, graphically:
 # use editor-canvas%, which is pretty, but not as yet obviously
 # function as text-field%, second is text-field%, which is
 # functional, but not as pretty as editor-canvas%
 #
 # issues:
 # - cannot figure out how to clear text from canvas
 # - default text for canvas% is preserved on resizing the window,
 #   which might be because of (send canvas on-draw).
 # - sending a new draw-text to canvas doesn't replace the default
 #   text, simply appears underneath the old.
 |#

; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight"]
                   [width 400]
                   [height 600]))

; make a static text message in the frame
(define frame-msg (new message% [parent frame]
                       [label "Blight"]
                       [min-width 40]))

#| ########## CANVAS AND OTHER FIELD DEFINITIONS ########## |#
; create a canvas object to draw stuff on
(define canvas (new canvas% [parent frame]
                    [min-height 400]
                    [vert-margin 5]
                    [style (list 'control-border 'no-autoclear
                                 'no-focus 'vscroll)]
                    [paint-callback
                     (λ (canvas dc)
                       (send dc set-scale 1 1)
                       (send dc set-text-foreground "black")
                       (send dc draw-text "" 0 0))]))
#|(define-values (canvas-vsize-x canvas-vsize-y)
  (send canvas get-virtual-size))
(send canvas init-auto-scrollbars canvas-vsize-x
      canvas-vsize-y 0.0 0.0)
(send canvas show-scrollbars #f #f) ; hide scrollbars|#

(define text (new text%
                  [line-spacing 1.0]
                  [auto-wrap #t]))

; an editor canvas where text% messages will appear
(define editor-canvas (new editor-canvas%
                           [parent frame]
                           [label "Your message goes here"]
                           [editor text]
                           [style (list 'control-border 'no-hscroll
                                        'auto-vscroll)]
                           [wheel-step 3]
                           [min-height 100]
                           [vert-margin 5]
                           [enabled #t]))
; make the window refresh more often
(send editor-canvas lazy-refresh #t)
(printf "Current editor for editor-canvas: ~a\n" (send editor-canvas get-editor))

; key event when the user presses Enter
(define enter-press (new key-event%
                         [key-code #\return]))

; create a text-field to enter a message
(define tfield (new text-field%
                    [label "Change title:"]
                    [parent frame]
                    [vert-margin 50]
                    [enabled #t]))

; panel for main frame
(define panel (new horizontal-panel%
                   [parent frame]))

; dialog box when exiting
(define exit-dialog (new dialog%
                         [label "Exit Blight"]
                         [style (list 'close-button)]))

; pane for buttons for exit-dialog so we get the buttons
; underneath the message (?) doesn't seem to want to work
#|(define exit-pane (new horizontal-pane%
                       [parent exit-panel]
                       [vert-margin 100]))|#

; dialog box when looking at Help
(define help-dialog (new dialog%
                         [label "About Blight"]
                         [style (list 'close-button)]))

; panel for help-dialog
(define help-panel (new horizontal-panel%
                        [parent help-dialog]))


; create a canvas object to draw stuff on - need a canvas
; to print the license message
(define help-canvas (new canvas% [parent help-panel]
                         [min-height 400]
                         [min-width 500]
                         [vert-margin 10]
                         [style (list 'control-border 'no-autoclear)]
                         [paint-callback
                          (λ (canvas dc)
                            (send dc set-scale 1 1)
                            (send dc set-text-foreground "black")
                            (send dc draw-text
                                  "Blight - a Tox client written in Racket." 0 0))]))

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
; uses message-box with 'ok-cancel
(new menu-item% [parent menu-file]
     [label "Quit"]
     [shortcut #\Q]
     [help-string "Quit Blight"]
     [callback (λ (button event)
                 ;(send exit-dialog show #t))])
                 (let ((mbox (message-box "Quit Blight"
                              "Are you sure you want to quit Blight?"
                              exit-dialog
                              (list 'ok-cancel))))
                   (if (eq? mbox 'ok)
                       (exit)
                       null)))])

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

#| ########### BUTTONS AND STUFF ################# |#
; panel button for sending the message from tfield
(new button% [parent panel]
     [label "Send Title"]
     [callback (λ (button event)
                 ; set new title to what's inside tfield
                 (send frame-msg set-label (send tfield get-value))
                 ; reset tfield to empty
                 (send tfield set-value ""))])

; panel button for sending text to canvas
; uses tfield
#|(new button% [parent panel]
     [label "Send Message"]
     [callback (λ (button event)
                 ; send canvas contents of tfield
                 (let ((dc (send canvas get-dc)))
                   ;(send dc draw-text "" 0 0) ; doesn't work?
                   (send dc draw-text (send tfield get-value) 0 0)
                   (send tfield set-value "")))])|#

; uses editor-canvas to draw to canvas
(new button% [parent panel]
     [label "Send Message"]
     [callback (λ (button event)
                 ; send canvas contents of editor-canvas
                 (let ((dc (send canvas get-dc)))
                   ;(send dc draw-text "" 0 0)
                   (send dc draw-text
                         (send text get-text 0 'eof #t #t)
                         0 0)))])

; clears the canvas
(new button% [parent panel]
     [label "Clear Canvas"]
     [callback (λ (button event)
                 ; send canvas contents of tfield
                 (let ((dc (send canvas get-dc)))
                   (send dc draw-text "" 0 0)
                   (send canvas flush)))])

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