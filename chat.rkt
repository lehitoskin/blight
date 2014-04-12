#lang racket/gui
; chat.rkt
; contains chat-window definitions
(provide all-from-out)

; create a new top-level window
; make a frame by instantiating the frame% class
(define chat-frame (new frame%
                   [label "Blight"]
                   [width 400]
                   [height 600]))

; make a static text message in the frame
(define chat-frame-msg (new message% [parent chat-frame]
                       [label "tox get friend name"]
                       [min-width 40]))

; key event when the user presses Enter
(define enter-press (new key-event%
                         [key-code #\return]))

; create a canvas object to draw stuff on
(define chat-canvas (new canvas% [parent chat-frame]
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

(define chat-text (new text%
                  [line-spacing 1.0]
                  [auto-wrap #t]))

; an editor canvas where text% messages will appear
(define chat-editor-canvas (new editor-canvas%
                           [parent chat-frame]
                           [label "Your message goes here"]
                           [editor chat-text]
                           [style (list 'control-border 'no-hscroll
                                        'auto-vscroll)]
                           [wheel-step 3]
                           [min-height 100]
                           [vert-margin 5]
                           [enabled #t]))
; make the window refresh more often
(send chat-editor-canvas lazy-refresh #t)