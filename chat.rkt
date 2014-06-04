#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket
         ffi/unsafe)
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

(define font-size-delta
  (make-object style-delta% 'change-size 10))

(define chat-window%
  (class frame%
    (inherit set-label)
    (init-field this-label
                this-width
                this-height
                this-tox)
    
    (define friend-name "")
    (define friend-key "")
    (define friend-num -1)
    
    ; create a new top-level window
    ; make a frame by instantiating the frame% class
    (define chat-frame (new frame%
                            [label this-label]
                            [width this-width]
                            [height this-height]))
    
    ; menu bar for chat-frame
    (define chat-frame-menu-bar (new menu-bar%
                                     [parent chat-frame]))
    
    ; menu File for menu bar
    (define menu-file (new menu% [parent chat-frame-menu-bar]
                           [label "&File"]
                           [help-string "Send, Call, etc."]))
    
    ; send a file to your friend
    (new menu-item% [parent menu-file]
         [label "Send File"]
         [help-string "Send a file to this friend"]
         [callback (λ (button event)
                     (printf "Tried to send a file to ~a!\n" friend-name)
                     ; create a new thread to send a file
                     #;(thread
                        (let* ((path (get-file "Select a file to send"))
                               (max-size (tox_file_data_size this-tox friend-num))
                               (length (file-size path))
                               (data (file->bytes path #:mode 'binary)))
                          (define data-ptr (malloc 'atomic (* max-size (ctype-sizeof _uint8_t))))
                          (do ((i 0 (+ i 1)))
                            ((= i length))
                            (ptr-set! data-ptr _uint8_t i (bytes-ref data i)))
                          (tox_file_send_data this-tox friend-num 0 data-ptr length)
                          (tox_file_send_control this-tox friend-num 1 0
                                                 (_TOX_FILECONTROL-index 'FINISHED) 0 0))))])
    
    ; close the current window
    (new menu-item% [parent menu-file]
         [label "&Close"]
         [shortcut #\W]
         [help-string "Close Window"]
         [callback (λ (button event)
                     (send this show #f))])
    
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
    (send chat-text-receive change-style font-size-delta)
    
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
    (send chat-text-send change-style font-size-delta)
    
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
                                   (bytes-length
                                    (string->bytes/utf-8 (send this-editor get-text))))
                 (send this-editor erase)
                 (send chat-text-send change-style font-size-delta))]
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
    
    (define/public (set-new-label x)
      (send chat-frame set-label x)
      (send chat-frame-msg set-label x))
    
    (define/override (show x)
      (send chat-frame show x))
    
    (define/override (is-shown?)
      (send chat-frame is-shown?))
    
    (define/override (is-enabled?)
      (send chat-frame is-enabled?))
    
    (define/public (set-name name)
      (set! friend-name name))
    
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
    
    (define/public (set-key key)
      (set! friend-key key))
    
    (define/public (get-key)
      friend-key)
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
