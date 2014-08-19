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
    
    (set-default-chatframe-bindings chatframe-keymap)
    
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
    
    (define invite-frame (new frame%
                              [label "Blight - Invite Friend"]
                              [width 200]
                              [height 400]))
    
    (define invite-list-box (new list-box%
                                 [parent invite-frame]
                                 [label "Friends"]
                                 [style (list 'single 'vertical-label)]
                                 [choices (list "")]
                                 [callback (λ (l e)
                                             (when (eq? (send e get-event-type)
                                                        'list-box-dclick)
                                               (let ((selection (send l get-selection)))
                                                 (invite-friend this-tox selection group-number)
                                                 (send invite-frame show #f))))]))
    
    (new button% [parent invite-frame]
         [label "&Cancel"]
         [callback (λ (button event)
                     (send invite-frame show #f))])
    
    (define/public update-invite-list
      (λ ()
        (define num-friends (friendlist-length this-tox))
        (unless (zero? num-friends)
          (send invite-list-box clear)
          (define friend-name-buf (make-bytes TOX_FRIEND_ADDRESS_SIZE))
          (define friend-key-buf (make-bytes TOX_CLIENT_ID_SIZE))
          ; loop until we get all our friends
          (do ((num 0 (+ num 1)))
            ((= num num-friends))
            (let* ((friend-name-length (get-name this-tox num friend-name-buf))
                   (friend-name-text (bytes->string/utf-8
                                      (subbytes friend-name-buf 0 friend-name-length))))
              ; add to the invite list
              (send invite-list-box append friend-name-text))))))
    (update-invite-list)
    
    (new menu-item% [parent menu-file]
         [label "Invite"]
         [help-string "Invite a friend"]
         [callback (λ (button event)
                     (send invite-frame show #t))])
    
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
    (send group-text-receive change-style black-style)
    
    (define (init-messages-keymap)
      (let ([km (new keymap%)])
        (send km add-function "copy"
              (lambda (editor kev)
                (send editor copy)))
        
        (send km add-function "backward-char"
              (lambda (editor kev)
                (send editor move-position 'left)))
        
        (send km add-function "select-all"
              (lambda (editor kev)
                (send editor move-position 'end)
                (send editor extend-position 0)))
        
        (send km add-function "backward-word"
              (lambda (editor kev)
                (send editor move-position 'left #f 'word)))
        
        (send km add-function "forward-char"
              (lambda (editor kev)
                (send editor move-position 'right)))
        
        (send km add-function "forward-word"
              (lambda (editor kev)
                (send editor move-position 'right #f 'word)))
        
        (send km add-function "previous-line"
              (lambda (editor kev)
                (send editor move-position 'up)))
        
        (send km add-function "next-line"
              (lambda (editor kev)
                (send editor move-position 'down)))
        
        (send km add-function "beginning-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'home)))
        
        (send km add-function "end-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'end)))
        
        (send km add-function "wheel-up"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'up))
                 (send (send editor get-canvas) wheel-step))))
        
        (send km add-function "wheel-down"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'down))
                 (send (send editor get-canvas) wheel-step))))
        km))
    
    (define messages-keymap (init-messages-keymap))
    
    (define (set-default-messages-bindings km)
      (send km map-function ":c:c" "copy")
      (send km map-function ":c:с" "copy") ;; russian cyrillic
      
      (send km map-function ":c:a" "select-all")
      (send km map-function ":c:ф" "select-all") ;; russian cyrillic
      
      (send km map-function ":left" "backward-char")
      (send km map-function ":right" "forward-char")
      (send km map-function ":c:left" "backward-word")
      (send km map-function ":c:right" "forward-word")
      (send km map-function ":up" "previous-line")
      (send km map-function ":down" "next-line")
      (send km map-function ":home" "beginning-of-buffer")
      (send km map-function ":end" "end-of-buffer")
      
      (send km map-function ":wheelup" "wheel-up")
      (send km map-function ":wheeldown" "wheel-down"))
    
    (set-default-messages-bindings messages-keymap)
    (send messages-keymap chain-to-keymap chatframe-keymap #t)
    
    (define custom-receive-canvas%
      (class editor-canvas%
        (inherit refresh get-dc get-size)
        (init-field parent
                    label
                    editor
                    style
                    wheel-step
                    min-height
                    vert-margin
                    this-chat-window)
        
        (define/public (get-chat-window) this-chat-window)
        
        (define/override (on-char key-event)
          
          (send messages-keymap handle-key-event editor key-event))
        
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

    (define custom-editor-canvas%
      (class editor-canvas%
        (inherit get-dc)
        (init-field this-parent
                    this-label
                    this-editor
                    this-style
                    this-wheel-step
                    this-min-height
                    this-vert-margin
                    this-chat-window)

        (define/public (get-chat-window) this-chat-window)
        ; TODO:
        ; unicode?
        ; wheel-up/wheel-down(?)
        (define/override (on-char key-event)

          (when (not (send editor-keymap handle-key-event this-editor key-event))
            (let ((key (send key-event get-key-code)))
              (when (char? (send key-event get-key-code))
                (send this-editor insert key)))))

        (super-new
         [parent this-parent]
         [label this-label]
         [editor this-editor]
         [style this-style]
         [wheel-step this-wheel-step]
         [min-height this-min-height]
         [vert-margin this-vert-margin]
         [stretchable-height #f])))

    (define group-text-send (new text%
                                 [line-spacing 1.0]
                                 [auto-wrap #t]))

    (define group-editor-canvas-receive (new custom-receive-canvas%
                                             [parent topside-panel]
                                             [label "Messages received"]
                                             [editor group-text-receive]
                                             [min-height 400]
                                             [min-width (- this-width 100)]
                                             [vert-margin 5]
                                             [style (list 'control-border 'no-hscroll
                                                          'auto-vscroll)]
                                             [wheel-step 3]
                                             [this-chat-window this]))

    ; an editor canvas where text% messages will appear
    (define group-editor-canvas-send (new custom-editor-canvas%
                                          [this-parent group-frame]
                                          [this-label "Your message goes here"]
                                          [this-editor group-text-send]
                                          [this-style (list 'control-border 'no-hscroll
                                                            'auto-vscroll)]
                                          [this-wheel-step 3]
                                          [this-min-height 100]
                                          [this-vert-margin 5]
                                          [this-chat-window this]))
    
    (define cur-focused-wdg group-editor-canvas-send)
    
    (define/public cycle-focus
      (λ (forward)
         (cond 
          [(eq? cur-focused-wdg group-editor-canvas-receive)
           (set! cur-focused-wdg group-editor-canvas-send)]
          [else (set! cur-focused-wdg group-editor-canvas-receive)])
         (send cur-focused-wdg focus)))
    
    (define group-list-box (new list-box%
                                [parent topside-panel]
                                [label "0 Peers   "]
                                [style (list 'single 'vertical-label)]
                                [stretchable-width 200]
                                [choices (list "Me")]
                                [callback (λ (list-box control-event)
                                            (match (send control-event get-event-type)
                                              ['list-box-dclick
                                               (define selection (send list-box get-selection))
                                               (define nick
                                                 (send list-box get-string selection))
                                               (send group-text-send insert nick)]
                                              [_ (void)]))]))
    
    (define panel (new horizontal-panel%
                       [parent group-frame]
                       [stretchable-height #f]
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
            ; parse for /commands
            (cond [(and (> (bytes-length byte-str) 4)
                        (bytes=? (subbytes byte-str 0 4) #"/me "))
                   (group-action-send this-tox group-number (subbytes byte-str 4)
                                      (bytes-length byte-str))]
                  [else (group-message-send this-tox group-number byte-str
                                            (bytes-length byte-str))])))
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
    
    (send group-text-send change-style black-style)
    
    ; guess I need to override some shit to get the keys just right
    
    (define (init-editor-keymap)
      (let ([km (new keymap%)])
        
        (send km add-function "insert-clipboard"
              (lambda (editor kev)
                (send editor paste)))
        
        (send km add-function "insert-primary"
              (lambda (editor kev)
                (send editor paste-x-selection)))
        
        (send km add-function "cut"
              (lambda (editor kev)
                (send editor cut)))
        
        (send km add-function "copy"
              (lambda (editor kev)
                (send editor copy)))
        
        (send km add-function "select-all"
              (lambda (editor kev)
                (send editor move-position 'end)
                (send editor extend-position 0)))
        
        (send km add-function "send-and-clear"
              (lambda (editor kev)
                (unless (string=? (send editor get-text) "")
                  (let ((msg-bytes (string->bytes/utf-8 (send editor get-text))))
                    (do-send-message editor msg-bytes)
                    (send editor erase)
                    (send editor change-style black-style)))))
        
        (send km add-function "insert-newline"
              (lambda (editor kev)
                (send editor insert "\n")))
        
        (send km add-function "delete-backward-char"
              (lambda (editor kev)
                (send editor delete)))
        
        (send km add-function "delete-forward-char"
              (lambda (editor kev)
                (send editor delete
                      (send editor get-start-position)
                      (+ (send editor get-end-position) 1))))
        
        (send km add-function "backward-char"
              (lambda (editor kev)
                (send editor move-position 'left)))
        
        (send km add-function "backward-word"
              (lambda (editor kev)
                (send editor move-position 'left #f 'word)))
        
        (send km add-function "forward-char"
              (lambda (editor kev)
                (send editor move-position 'right)))

        (send km add-function "mark-char-backward"
              (lambda (editor kev)
                (let ([cur (send editor get-start-position)])
                  (send editor move-position 'left #t 'simple))))

        (send km add-function "mark-char"
              (lambda (editor kev)
                (let ([cur (send editor get-start-position)])
                  (send editor move-position 'right #t 'simple))))
        
        (send km add-function "forward-word"
              (lambda (editor kev)
                (send editor move-position 'right #f 'word)))

        (send km add-function "backward-kill-word"
              (lambda (editor kev)
                (let ([to (send editor get-start-position)])
                  (send editor move-position 'left #f 'word)
                  (let ([from (send editor get-start-position)])
                    (send editor delete
                          from to)))))

        (send km add-function "forward-kill-word"
              (lambda (editor kev)
                (let ([from (send editor get-start-position)])
                  (send editor move-position 'right #f 'word)
                  (let ([to (send editor get-start-position)])
                    (send editor delete
                          from to)))))
        
        (send km add-function "previous-line"
              (lambda (editor kev)
                (send editor move-position 'up)))
        
        (send km add-function "next-line"
              (lambda (editor kev)
                (send editor move-position 'down)))
        
        (send km add-function "beginning-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'home)))
        
        (send km add-function "end-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'end)))
        
        (send km add-function "wheel-up"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'up))
                 (send (send editor get-canvas) wheel-step))))
        
        (send km add-function "wheel-down"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'down))
                 (send (send editor get-canvas) wheel-step))))
        
        (send km add-function "special-insert-symbol"
              (lambda (editor kev)
                (let ((key (send kev get-key-code))
                      (control (send kev get-control-down))
                      (shift (send kev get-shift-down))
                      (alt (send kev get-alt-down)))
                  (cond
                   [(and (eqv? key #\\) (eq? control #t))
                    (send editor insert "\u03BB")] ; λ
                   [(and (eqv? key #\1) (eq? control #t))
                    (send editor insert "\u00A9")] ; copyright
                   [(and (eqv? key #\2) (eq? control #t))
                    (send editor insert "\u00AE")] ; registered-trademark
                   [(and (eqv? key #\3) (eq? control #t))
                    (send editor insert "\u2122")] ; trademark
                   ))))
        km))
    
    (define editor-keymap (init-editor-keymap))
    
    (define (set-default-editor-bindings km)
      (send km map-function ":c:c" "copy")
      (send km map-function ":c:с" "copy") ;; russian cyrillic
      (send km map-function ":c:v" "insert-clipboard")
      (send km map-function ":c:м" "insert-clipboard") ;; russian cyrillic
      (send km map-function ":c:x" "cut")
      (send km map-function ":c:ч" "cut") ;; russian cyrillic
      (send km map-function ":c:a" "select-all")
      (send km map-function ":c:ф" "select-all") ;; russian cyrillic
      (send km map-function "~s:return" "send-and-clear")
      (send km map-function ":s:return" "insert-newline")
      (send km map-function ":numpadenter" "insert-newline")
      (send km map-function ":backspace" "delete-backward-char")
      (send km map-function ":delete" "delete-forward-char")
      (send km map-function ":left" "backward-char")
      (send km map-function ":right" "forward-char")
      (send km map-function ":c:left" "backward-word")
      (send km map-function ":c:right" "forward-word")
      (send km map-function ":c:backspace" "backward-kill-word")
      (send km map-function ":c:delete" "forward-kill-word")
      (send km map-function ":s:left" "mark-char-backward")
      (send km map-function ":s:right" "mark-char")
      (send km map-function ":up" "previous-line")
      (send km map-function ":down" "next-line")
      (send km map-function ":home" "beginning-of-buffer")
      (send km map-function ":end" "end-of-buffer")
      (send km map-function ":wheelup" "wheel-up")
      (send km map-function ":wheeldown" "wheel-down")
      (send km map-function ":middlebutton" "insert-primary")
      (send km map-function ":s:insert" "insert-primary")
      (send km map-function ":c:1" "special-insert-symbol")
      (send km map-function ":c:2" "special-insert-symbol")
      (send km map-function ":c:3" "special-insert-symbol")
      (send km map-function ":c:\\" "special-insert-symbol"))
    
    (set-default-editor-bindings editor-keymap)
    (send editor-keymap chain-to-keymap chatframe-keymap #t)
    

    (define/public (set-new-label x)
      (send group-frame set-label x)
      (send group-frame-msg set-label x))
    
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
    
    (define/public (set-group-number num)
      (set! group-number num))
    
    (define/public (get-list-box)
      group-list-box)
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
