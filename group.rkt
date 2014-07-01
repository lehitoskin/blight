#lang racket/gui
; group.rkt
; contains group-window definitions
(require libtoxcore-racket/functions
         "helpers.rkt"
         "number-conversions.rkt"
         "config.rkt"
         "chat.rkt")
(provide (all-defined-out))

(define group-window%
  (class frame%
    (inherit set-label)
    (init-field this-label
                this-width
                this-height
                this-tox
                group-number)
    
    (define group-name "")
    
    (define/private repeat
      (λ (proc times)
        (cond [(zero? times) #t]
              [else (proc) (repeat proc (- times 1))])))
    
    ; create a new top-level window
    ; make a frame by instantiating the frame% class
    (define group-frame (new frame%
                             [label this-label]
                             [width this-width]
                             [height this-height]))
    
    ; menu bar for group-frame
    (define group-frame-menu-bar (new menu-bar%
                                      [parent group-frame]))
    
    ; menu File for menu bar
    (define menu-file (new menu% [parent group-frame-menu-bar]
                           [label "&File"]))
    
    (new menu-item% [parent menu-file]
         [label "Invite"]
         [help-string "Invite a friend"]
         [callback (λ (button event)
                     (displayln "Tried to invite a friend!"))])
    
    ; close the current window
    (new menu-item% [parent menu-file]
         [label "&Close"]
         [shortcut #\W]
         [help-string "Close Window"]
         [callback (λ (button event)
                     (send this show #f))])
    
    (define group-frame-msg (new message%
                                 [parent group-frame]
                                 [label this-label]
                                 [min-width 40]))
    (send group-frame-msg auto-resize #t)
    
    (define group-text-receive (new text%
                                    [line-spacing 1.0]
                                    [auto-wrap #t]))
    (send group-text-receive change-style font-size-delta)
    
    (define custom-receive-canvas%
      (class editor-canvas%
        (inherit refresh get-dc get-size)
        (init-field parent
                    label
                    editor
                    style
                    wheel-step
                    min-height
                    vert-margin)
        (define/override (on-char key-event)
          (let ((key (send key-event get-key-code))
                (control (send key-event get-control-down)))
            (cond
              ; ^c - copy selected text to clipboard
              [(and (eqv? key #\c) (eq? control #t)) (send editor copy)]
              ; scroll up/scroll down
              [(eqv? key 'wheel-up) (repeat
                                     (λ () (send editor move-position 'up))
                                     wheel-step)]
              [(eqv? key 'wheel-down) (repeat
                                       (λ ()(send editor move-position 'down))
                                       wheel-step)])))
        (super-new
         [parent parent]
         [label label]
         [editor editor]
         [style style]
         [wheel-step wheel-step]
         [min-height min-height]
         [vert-margin vert-margin])))
    
    (define topside-panel (new horizontal-panel%
                               [parent group-frame]))
    
    (define group-editor-canvas-receive (new custom-receive-canvas%
                                             [parent topside-panel]
                                             [label "Messages received"]
                                             [editor group-text-receive]
                                             [min-height 400]
                                             [vert-margin 5]
                                             [style (list 'control-border 'no-hscroll
                                                          'auto-vscroll)]
                                             [wheel-step 3]
                                             [min-width (- this-width 100)]))
    
    (define group-list-box (new list-box%
                                [parent topside-panel]
                                [label "Name"]
                                [style (list 'single 'vertical-label)]
                                [choices (list "Me")]))
    
    (define panel (new horizontal-panel%
                       [parent group-frame]
                       [alignment '(right center)]))
    
    (define emoji-button (new button%
                              [parent panel]
                              [label "Enter Emoji"]
                              [callback (λ (button event)
                                          (send emoji-dialog show #t))]))
    
    #| ##################### BEGIN EMOJI STUFF #################### |#
    (define emoji-dialog (new dialog%
                              [label "Blight - Select Emoji"]
                              [style (list 'close-button)]))
    
    ; first row of emoji
    (define row-one (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ([i (in-range 6)])
      (make-object button%
        (format "~a" (list-ref emojis i))
        row-one
        (λ (button event)
          (send group-text-send insert (list-ref emojis i))
          (send emoji-dialog show #f))))
    
    ; second row of emoji
    (define row-two (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ([i (in-range 6)])
      (make-object button%
        (format "~a" (list-ref emojis (+ i 6)))
        row-two
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 6)))
          (send emoji-dialog show #f))))
    
    ; third row of emoji
    (define row-three (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ([i (in-range 6)])
      (make-object button%
        (format "~a" (list-ref emojis (+ i 12)))
        row-three
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 12)))
          (send emoji-dialog show #f))))
    
    ; fourth row of emoji
    (define row-four (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 18)))
        row-four
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 18)))
          (send emoji-dialog show #f))))
    
    ; fifth row of emoji
    (define row-five (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 24)))
        row-five
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 24)))
          (send emoji-dialog show #f))))
    
    ; sixth row of emoji
    (define row-six (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 30)))
        row-six
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 30)))
          (send emoji-dialog show #f))))
    
    ; seventh row of emoji
    (define row-seven (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 36)))
        row-seven
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 36)))
          (send emoji-dialog show #f))))
    
    ; eighth row of emoji
    (define row-eight (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 42)))
        row-eight
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 42)))
          (send emoji-dialog show #f))))
    
    ; ninth row of emoji
    (define row-nine (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 48)))
        row-nine
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 48)))
          (send emoji-dialog show #f))))
    
    ; tenth and final row of emoji
    (define row-ten (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 54)))
        row-ten
        (λ (button event)
          (send group-text-send insert (list-ref emojis (+ i 54)))
          (send emoji-dialog show #f))))
    
    (new button%
         [parent emoji-dialog]
         [label "Close"]
         [callback (λ (button event)
                     (send emoji-dialog show #f))])
    #| #################### END EMOJI STUFF #################### |#
    
    ; procedure to inspect a sending message and split it so it does not
    ; exceed TOX_MAX_MESSAGE_LENGTH
    (define split-bytes
      (λ (msg-bytes)
        (let ((msg-length (bytes-length msg-bytes)))
          (define max-length TOX_MAX_MESSAGE_LENGTH)
          ; check if number of bytes exceeds max by more than twice
          ; currently only supports up to (* max-length 3) message length
          ; but that should totally be enough for a single message
          (cond [(> msg-length (* max-length 2))
                 (define port (open-input-bytes msg-bytes port))
                 ; return three values
                 (values (peek-bytes max-length 0 port)
                         (peek-bytes max-length max-length port)
                         (if (zero? (remainder (bytes-length msg-bytes) max-length))
                             (peek-bytes max-length (* max-length 2) port)
                             (peek-bytes (remainder (bytes-length msg-bytes) max-length)
                                         (* max-length 2) port)))]
                [else (define port (open-input-bytes msg-bytes))
                      ; return two values
                      (values (peek-bytes max-length 0 port)
                              (if (zero? (remainder (bytes-length msg-bytes) max-length))
                                  (peek-bytes max-length max-length port)
                                  (peek-bytes (remainder (bytes-length msg-bytes) max-length)
                                              max-length port)))]))))
    
    ; send the message to the editor and then through tox
    ; assumes msg is already a byte-string
    (define do-send-message
      (λ (editor msg-bytes)
        ; procedure to send to the editor and to tox
        (define do-send
          (λ (byte-str)
            ; only need to send message through tox, will echo through callback
            (group-message-send this-tox group-number msg-bytes (bytes-length byte-str))))
        (cond [(> (bytes-length msg-bytes) (* TOX_MAX_MESSAGE_LENGTH 2))
               ; if the message is greater than twice our max length, split it
               ; into three chunks
               (define-values (first-third second-third third-third)
                 (split-bytes msg-bytes))
               ; send first third
               (do-send first-third)
               (do-send second-third)
               (do-send third-third)]
              [(and (> (bytes-length msg-bytes) TOX_MAX_MESSAGE_LENGTH)
                    (<= (bytes-length msg-bytes) (* TOX_MAX_MESSAGE_LENGTH 2)))
               ; if the message is greater than our max length, but less than
               ; or equal to twice the max length, split it into two chunks
               (define-values (first-half second-half) (split-bytes msg-bytes))
               (do-send first-half)
               (do-send second-half)]
              [else (do-send msg-bytes)])))
    
    (define group-text-send (new text%
                                [line-spacing 1.0]
                                [auto-wrap #t]))
    (send group-text-send change-style font-size-delta)
    
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
                 (let ((msg-bytes (string->bytes/utf-8 (send this-editor get-text))))
                   (do-send-message this-editor msg-bytes)
                   (send this-editor erase)
                   (send this-editor change-style font-size-delta)))]
              ; shift-enter adds a newline to the text area
              [(and (eqv? key #\return) (eq? shift #t)) (send this-editor insert "\n")]
              ; enter on the keypad adds a newline to the text area
              [(eqv? key #\u0003) (send this-editor insert "\n")]
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
              [(eqv? key 'wheel-up) (repeat
                                     (λ () (send this-editor move-position 'up))
                                     this-wheel-step)]
              [(eqv? key 'wheel-down) (repeat
                                       (λ ()(send this-editor move-position 'down))
                                       this-wheel-step)]
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
    (define group-editor-canvas-send (new custom-editor-canvas%
                                         [this-parent group-frame]
                                         [this-label "Your message goes here"]
                                         [this-editor group-text-send]
                                         [this-style (list 'control-border 'no-hscroll
                                                           'auto-vscroll)]
                                         [this-wheel-step 3]
                                         [this-min-height 100]
                                         [this-vert-margin 5]))
    
    (define/public (set-new-label x)
      (send group-frame set-label x)
      (send group-frame-msg set-label x))
    
    (define/public (get-name x)
      group-name)
    
    (define/public (set-name x)
      (set! group-name x))
    
    (define/override (show x)
      (send group-frame show x))
    
    (define/override (is-shown?)
      (send group-frame is-shown?))
    
    (define/override (is-enabled?)
      (send group-frame is-enabled?))
    
    (define/public (get-receive-editor)
      (send group-editor-canvas-receive get-editor))
    
    (define/public (get-send-editor)
      (send group-editor-canvas-send get-editor))
    
    (define/public (get-group-number)
      group-number)
    
    (define/public (get-list-box)
      group-list-box)
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
