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
 # - need to reimplement a whole bunch of key events because we're
 #   overriding this, baby!
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

; guess I need to override some shit to get the keys just right
(define custom-editor-canvas%
  (class editor-canvas%
    (inherit get-dc)
    (init-field this-parent
                this-label
                this-editor
                this-style
                this-wheel-step
                this-min-height
                this-vert-margin)
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
    ; key event for shift+enter
    ; so the user can do a newline without sending
    ; the message early
    (define shift-enter-press (new key-event%
                                   [key-code #\return]
                                   [shift-down #t]))
    (define all-press (new key-event%
                           [key-code #\a]
                           [control-down #t]))
    ; TODO:
    ; ^c
    ; ^p
    ; ^x
    ; ^a
    ; #\ENTER (?)
    ; unicode? on-release, not on-char
    ; wheel-up/wheel-down
    (define/override (on-char key-event)
      (let ((key (send key-event get-key-code)))
        (cond
          ; keyboard shortcuts
          ; ^c - copy selected text to clipboard
          ; doesn't work
          ; need to properly compare key-event with copy-press
          #|[(equal? key-event copy-press) (send this-editor
                                               copy-self-to
                                               chat-clipboard)
                                         (displayln "Copy-press!")]
          ; ^a - select all text in text area
          [(equal? key-event all-press) (send this-editor move-position 'end)
                                        (send this-editor extend-position 0)]|#
          ; press enter - send the message/clear text area
          [(eqv? key #\return) (unless (string=? (send this-editor get-text) "")
                                 (send chat-text-receive insert
                                       (string-append "Me: "
                                                      (send this-editor get-text)
                                                      "\n"))
                                 (send this-editor erase))]
          ; press backspace, delete previous character or selected text
          [(eqv? key #\backspace) (send this-editor delete)]
          ; press delete, delete proceding character or selected text
          [(eqv? key #\rubout) (send this-editor delete
                                     (send this-editor get-start-position)
                                     (+ (send this-editor get-end-position) 1))]
          ; press enter key on numpad - newline is added to text area
          [(eqv? key 'numpad-enter) (send this-editor insert "\n")]
          ; navigate through text area
          [(eqv? key 'left) (send this-editor move-position 'left)]
          [(eqv? key 'right) (send this-editor move-position 'right)]
          [(eqv? key 'up) (send this-editor move-position 'up)]
          [(eqv? key 'down) (send this-editor move-position 'down)]
          [(eqv? key 'home) (send this-editor move-position 'home)]
          [(eqv? key 'end) (send this-editor move-position 'end)]
          [(eqv? key 'wheel-up) (send this-editor on-char key-event)]
          [(eqv? key 'wheel-down) (send this-editor on-char key-event)]
          ; regular key pressed, add to text area
          [(char? key) (send this-editor insert key)])))
    (super-new
     [parent this-parent]
     [label this-label]
     [editor this-editor]
     [style this-style]
     [wheel-step this-wheel-step]
     [min-height this-min-height]
     [vert-margin this-vert-margin])))

; an editor canvas where text% messages will appear
(define chat-editor-canvas-send (new custom-editor-canvas%
                                     [this-parent chat-frame]
                                     [this-label "Your message goes here"]
                                     [this-editor chat-text-send]
                                     [this-style (list 'control-border 'no-hscroll
                                                       'auto-vscroll)]
                                     [this-wheel-step 3]
                                     [this-min-height 100]
                                     [this-vert-margin 5]))

#|(define chat-tfield-send (new text-field%
                              [parent chat-frame]
                              [label ""]
                              [min-height 80]
                              [style (list 'single)]
                              [callback (λ (l e)
                                          (when (eq? (send e get-event-type)
                                                     'text-field-enter)
                                            (let ((text (send l get-value)))
                                              (unless (string=? text "")
                                                (send chat-text-receive insert
                                                      (string-append "Me: "
                                                                     text
                                                                     "\n"))
                                                (send l set-value "")))
                                            (send enter-press get-key-code)))]))
(define chat-text-send (send chat-tfield-send get-editor))|#

; chat-panel for main chat-frame
#|(define chat-panel (new horizontal-panel%
                        [parent chat-frame]))

; uses editor-canvas to draw to canvas
(new button% [parent chat-panel]
     [label "Send Message"]
     [callback (λ (button event)
                 ; send to chat-editor-canvas-receive the
                 ; contents of chat-tfield-send
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
                 (send chat-text-send erase))])|#
