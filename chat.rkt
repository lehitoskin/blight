#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require "helpers.rkt")
(provide (all-defined-out))

#|
 # two ways (so far) to go about sending a message, graphically:
 # use editor-canvas%, which is pretty, but not as yet obviously
 # function as text-field%, second is text-field%, which is
 # functional, but not as pretty as editor-canvas%
 #
 # issues:
 # - still needs to send when user presses enter
 #   as well as needs functionality for Shift-Enter
 |#

; create a new top-level window
; make a frame by instantiating the frame% class
(define chat-frame (new frame%
                        [label "Blight"]
                        [width 400]
                        [height 600]))

; make a static text message in the frame
; replaced immediately by list-box from buddy list
(define chat-frame-msg (new message% [parent chat-frame]
                            [label "Friend Name"]
                            [min-width 40]))

; clipboard control thingie
(define chat-clipboard-client (new clipboard-client%))
(define chat-clipboard the-clipboard)
(send chat-clipboard-client add-type "TEXT")

; key event when the user presses Enter
(define enter-press (new key-event%
                         [key-code #\return]))

; key event for ctrl+c
(define copy-press (new key-event%
                        [key-code #\c]
                        [control-down #t]))

; key event for ctrl+v
(define paste-press (new key-event%
                         [key-code #\v]
                         [control-down #t]))

; key event for ctrl+x
(define cut-press (new key-event%
                       [key-code #\x]
                       [control-down #t]))

(define chat-text-receive (new text%
                               [line-spacing 1.0]
                               [auto-wrap #t]))

(define chat-editor-canvas-receive (new editor-canvas%
                                        [parent chat-frame]
                                        [label "Messages received"]
                                        [editor chat-text-receive]
                                        [min-height 400]
                                        [vert-margin 5]
                                        [style (list 'control-border 'no-hscroll
                                                     'auto-vscroll 'no-focus)]
                                        [wheel-step 3]))

(define chat-text-send (new text%
                            [line-spacing 1.0]
                            [auto-wrap #t]))

; an editor canvas where text% messages will appear
(define chat-editor-canvas-send (new editor-canvas%
                                     [parent chat-frame]
                                     [label "Your message goes here"]
                                     [editor chat-text-send]
                                     [style (list 'control-border 'no-hscroll
                                                  'auto-vscroll)]
                                     [wheel-step 3]
                                     [min-height 100]
                                     [vert-margin 5]
                                     [enabled #t]))

; chat-panel for main chat-frame
(define chat-panel (new horizontal-panel%
                        [parent chat-frame]))

; uses editor-canvas to draw to canvas
(new button% [parent chat-panel]
     [label "Send Message"]
     [callback (λ (button event)
                 ; send to chat-editor-canvas-receive the
                 ; contents of chat-editor-canvas-send
                 (send chat-text-receive insert
                       (string-append "Me: "
                                      (send chat-text-send get-text 0 'eof #t #t)
                                      "\n"))
                 ; is it an http?/https?
                 ; useful for link coloring etc.
                 #|(send chat-text-receive insert
                       (string-append "HTTP? "
                                      (if (http? (send chat-text-send get-text 0 'eof #t #t))
                                          "True"
                                          "False")
                                      "\n"))|#
                 ; clear chat-editor-canvas-send
                 (send chat-text-send erase))])
