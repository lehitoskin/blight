#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket)
(provide (all-defined-out))

#|
 # issues:
 # - need to reimplement a whole bunch of key events because we're
 #   overriding this, baby!
 |#

; clipboard control thingie
(define chat-clipboard-client (new clipboard-client%))
(define chat-clipboard the-clipboard)
(send chat-clipboard-client add-type "TEXT")

(define chat-window%
  (class frame%
    (inherit set-label)
    (init-field this-label
                this-width
                this-height
                this-tox
                friend-num)
    (define friend-name "")
    ; create a new top-level window
    ; make a frame by instantiating the frame% class
    (define chat-frame (new frame%
                            [label this-label]
                            [width this-width]
                            [height this-height]))
    
    ; make a static text message in the frame
    ; replaced immediately by list-box from buddy list
    (define chat-frame-msg (new message%
                                [parent chat-frame]
                                [label this-label]
                                [min-width 40]))
    (send chat-frame-msg auto-resize #t)
    
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
        ; TODO:
        ; fix 'numpad-enter
        ; unicode?
        ; wheel-up/wheel-down(?)
        (define/override (on-char key-event)
          (let ((key (send key-event get-key-code))
                (control (send key-event get-control-down))
                (shift (send key-event get-shift-down))
                (alt (send key-event get-alt-down)))
            (cond
              ; keyboard shortcuts
              ; ^c - copy selected text to clipboard
              [(and (eqv? key #\c) (eq? control #t)) (send this-editor copy)]
              ; ^v - paste text to text area
              [(and (eqv? key #\v) (eq? control #t)) (send this-editor paste)]
              ; ^x - cut selected text
              [(and (eqv? key #\x) (eq? control #t)) (send this-editor cut)]
              ; ^a - select all text in text area
              [(and (eqv? key #\a) (eq? control #t)) (send this-editor move-position 'end)
                                                     (send this-editor extend-position 0)]
              ; press enter - send the message/clear text area
              [(and (eqv? key #\return) (eq? shift #f))
               (unless (string=? (send this-editor get-text) "")
                 (send chat-text-receive insert
                       (string-append "Me: "
                                      (send this-editor get-text)
                                      "\n"))
                 (tox_send_message this-tox
                                   friend-num
                                   (send this-editor get-text)
                                   (string-length (send this-editor get-text)))
                 (send this-editor erase))]
              ; shift-enter adds a newline to the text area
              [(and (eqv? key #\return) (eq? shift #t)) (send this-editor insert "\n")]
              ; press backspace, delete previous character or selected text
              [(eqv? key #\backspace) (send this-editor delete)]
              ; press delete, delete proceding character or selected text
              [(eqv? key #\rubout) (send this-editor delete
                                         (send this-editor get-start-position)
                                         (+ (send this-editor get-end-position) 1))]
              ; other keyboard shortcuts
              [(and (eqv? key #\\) (eq? control #t))
               (send this-editor insert "\u03BB")] ; λ
              [(and (eqv? key #\1) (eq? control #t))
               (send this-editor insert "\u00A9")] ; copyright
              [(and (eqv? key #\2) (eq? control #t))
               (send this-editor insert "\u00AE")] ; registered-trademark
              [(and (eqv? key #\3) (eq? control #t))
               (send this-editor insert "\u2122")] ; trademark
              ; press enter key on numpad - newline is added to text area
              [(eqv? key 'numpad-enter) (send this-editor insert "\n")]
              ; navigate through text area
              [(eqv? key 'left) (send this-editor move-position 'left)]
              [(eqv? key 'right) (send this-editor move-position 'right)]
              [(eqv? key 'up) (send this-editor move-position 'up)]
              [(eqv? key 'down) (send this-editor move-position 'down)]
              [(eqv? key 'home) (send this-editor move-position 'home)]
              [(eqv? key 'end) (send this-editor move-position 'end)]
              ; janky workaround because implementing wheel scrolling is a PITA
              [(eqv? key 'wheel-up) (send this-editor move-position 'up)]
              [(eqv? key 'wheel-down) (send this-editor move-position 'down)]
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
    
    (define (set-new-label x)
      (send chat-frame set-label x)
      (send chat-frame-msg set-label x))
    
    (define/override (show x)
      (send chat-frame show x))
    
    (define/override (is-shown?)
      (send chat-frame is-shown?))
    
    (define/override (is-enabled?)
      (send chat-frame is-enabled?))
    
    (define/public (set-name name)
      (set! friend-name name)
      (set-new-label name))
    
    (define/public (get-name)
      friend-name)
    
    (define/public (get-receive-editor)
      (send chat-editor-canvas-receive get-editor))
    
    (define/public (get-send-editor)
      (send chat-editor-canvas-send get-editor))
    
    (define/public (set-friend-num num)
      (set! friend-num num))
    
    (define/public (get-friend-num)
      friend-num)
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
