#lang racket/gui
; group.rkt
; contains group-window definitions
(require libtoxcore-racket/functions
         "helpers.rkt"
         "number-conversions.rkt"
         "config.rkt"
         "chat.rkt"
         "msg-editor.rkt"
         "msg-history.rkt"
         )
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
    
    (define messages-keymap (init-messages-keymap this))
    
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

    (define message-history (new message-history%
                                 [editor group-text-receive]))


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

    (define/public set-editor-black-style
      (λ (editor)
         (send editor change-style black-style)
         ))

    
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
    
    ; send the message through tox and then add to history
    (define/public do-send-message
      (λ (editor message)
      ; get message type
      (define msg-type
        (send message-history get-msg-type message))

      (define msg-bytes (string->bytes/utf-8 message))      
         
        ; procedure to send to the editor and to tox
        (define do-send
          (λ (byte-str)
             (cond
             ; we're sending an action!
             [(eq? msg-type 'action)
              (group-action-send this-tox group-number (subbytes byte-str 4)
                                      (bytes-length byte-str))]
             ; we're not doing anything special
             [else (group-message-send this-tox group-number byte-str
                                            (bytes-length byte-str))])))
        
        ; split the message if it exceeds TOX_MAX_MESSAGE_LENGTH
        ; otherwise, just send it.
        (define split-message
          (λ (msg-bytes)
            (let ([len (bytes-length msg-bytes)])
              (cond [(<= len TOX_MAX_MESSAGE_LENGTH)
                     (do-send msg-bytes)]
                    [(> len TOX_MAX_MESSAGE_LENGTH)
                     (do-send (subbytes msg-bytes 0 TOX_MAX_MESSAGE_LENGTH))
                     (split-message (subbytes msg-bytes TOX_MAX_MESSAGE_LENGTH))]))))
        
        (split-message msg-bytes)))
    
    (send group-text-send change-style black-style)
    
    ; guess I need to override some shit to get the keys just right
    
    (define editor-keymap (init-editor-keymap this))
    
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

    (define/public (get-msg-history)
      message-history)
    
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
