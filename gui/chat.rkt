#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket
         (only-in racket/flonum
                  fl->exact-integer)
         "msg-editor.rkt"
         "msg-history.rkt"
         "../helpers.rkt"
         "../history.rkt"
         "../config.rkt"
         "../utils.rkt"
         (only-in pict
                  bitmap
                  scale-to-fit
                  pict->bitmap))
(provide (all-defined-out)
         fl->exact-integer
         bitmap
         scale-to-fit
         pict->bitmap)

; clipboard control thingie
(define chat-clipboard-client (new clipboard-client%))
(define chat-clipboard the-clipboard)
(send chat-clipboard-client add-type "TEXT")

(define (init-chatframe-keymap)
  (let ([km (new keymap%)])
    (send km add-function "close-chatframe"
          (lambda (wdg kev)
            
            (let ([chatframe (cond
                               ; key event from editor%
                               [(is-a? wdg text%) (send (send wdg get-canvas) get-parent)]
                               ; key event from frame itself
                               [(subclass? wdg frame%) wdg])])
              (send chatframe show #f))))
    
    (send km add-function "switch-focus"
          (lambda (wdg kev)
            (let ([chat-window
                   (cond [(is-a? wdg text%)
                          (send (send wdg get-canvas) get-chat-window)])])
              (send chat-window cycle-focus #t))))
    km))

(define chatframe-keymap (init-chatframe-keymap))

(define (set-default-chatframe-bindings km)
  (send km map-function ":c:w" "close-chatframe")
  (send km map-function ":c:ц" "close-chatframe") ; russian cyrillic
  (send km map-function ":c:tab" "switch-focus"))

(define chat-window%
  (class frame%
    (inherit set-label)
    (init-field this-label
                this-width
                this-height
                avatar-height
                avatar-width
                this-tox)
    
    (set-default-chatframe-bindings chatframe-keymap)
    
    (define fname "")
    (define friend-key "")
    (define friend-num -1)
    ; obtain our tox id
    (define my-id-bytes (self-address this-tox))
    (define my-id-hex (bytes->hex-string my-id-bytes))
    (define friend-avatar (make-bitmap 40 40))
    (define friend-has-avatar? #f)
    (define msg-read? #t)
    
    ; the sending file transfer list and its path list
    ; easier to have two lists than deal with a list of pairs
    
    ;; (define/private repeat
    ;;   (λ (proc times)
    ;;     (cond [(zero? times) #t]
    ;;           [else (proc) (repeat proc (- times 1))])))
    
    (define add-file-sender
      (λ (path filenumber)
        (define filename (path->string path))
        (transfers-read-file! filenumber)))
    
    (define data-control
      (λ (filenumber type)
        (file-control this-tox friend-num filenumber type)))
    
    (define custom-frame%
      (class frame%
        (inherit set-label
                 set-icon
                 show
                 is-shown?
                 has-focus?
                 is-enabled?
                 enable)
        (super-new)
        
        ; tell the chat frame to change the chat window title
        ; when msg-read? is #f
        (define/override (on-subwindow-focus receiver event)
          (when (and (false? msg-read?) event)
            (set! msg-read? #t)
            (set-new-label (string-append "Blight - " fname))))))
    
    ; create a new top-level window
    ; make a frame by instantiating the frame% class
    (define chat-frame (new custom-frame%
                            [label this-label]
                            [width this-width]
                            [height this-height]))
    
    ; set the frame icon
    (let ([icon-bmp (make-bitmap 32 32)])
      (send icon-bmp load-file logo)
      (send chat-frame set-icon icon-bmp))
    
    ; menu bar for chat-frame
    (define chat-frame-menu-bar (new menu-bar%
                                     [parent chat-frame]))
    
    ; menu File for menu bar
    (define menu-file (new menu% [parent chat-frame-menu-bar]
                           [label "&File"]
                           [help-string "Send, Call, etc."]))
    
    ; send a file to our friend
    (new menu-item%
         [parent menu-file]
         [label "Send File"]
         [help-string "Send a file to this friend"]
         [callback (λ (button event)
                     ; create a new thread so we don't get disconnected
                     (thread
                      (λ ()
                        (let ([path (get-file "Select a file to send")])
                          (unless (false? path)
                            ; total size of the file
                            (define size (file-size path))
                            ; name of the file (taken from the path)
                            (define filename (path->bytes (last (explode-path path))))
                            ; let the core determine the file id
                            (define-values (filenumber file-err)
                              (file-send this-tox friend-num
                                         'data size #"" filename))
                            ; get the file id created by the core
                            (define-values (id-success id-err f-id)
                              (file-id this-tox friend-num filenumber))
                            (transfers-add! this-tox friend-num filenumber f-id path
                                            (file->bytes path)))))))])
    
    (new menu-item% [parent menu-file]
         [label "File Controls"]
         [help-string "Control active file transfers"]
         [callback (λ (button event)
                     (send file-control-dialog show #t))])
    
    ; control active transfers
    (define file-control-dialog (new dialog%
                                     [label (format "~a - File Control" this-label)]
                                     [width 300]
                                     [height 200]))
    
    (define fc-list-box
      (new list-box%
           [parent file-control-dialog]
           [label "Files Available for Control"]
           [style '(single vertical-label)]
           [choices (list "")]))
    
    (define fc-rbox
      (new radio-box%
           [parent file-control-dialog]
           [label #f]
           [choices (list "Resume"
                          "Pause"
                          "Cancel")]))
    
    (define fc-button-hpanel
      (new horizontal-panel%
           [parent file-control-dialog]
           [alignment '(right center)]))
    
    (define fc-cancel-button
      (new button%
           [parent fc-button-hpanel]
           [label "Cancel"]
           [callback (λ (button event)
                       (send file-control-dialog show #f))]))
    
    (define fc-ok-button
      (new button%
           [parent fc-button-hpanel]
           [label "OK"]
           [callback (λ (button event)
                       (let* ([fc-lb fc-list-box]
                              [fc-rb fc-rbox]
                              [filenumber (send fc-lb get-selection)]
                              [sel (send fc-rb get-selection)]
                              [control-type (string->symbol
                                             (string-downcase
                                              (send fc-rb get-item-label sel)))])
                         (cond
                           ; no file transfers going on, do nothing
                           [(hash-empty? transfers)]
                           ; cancel the transfer
                           [(eq? control-type 'cancel)
                            (define-values (id-success id-err f-id)
                              (file-id this-tox friend-num filenumber))
                            (data-control filenumber control-type)
                            (when (hash-has-key? transfers f-id)
                              (transfers-del! f-id))]
                           ; receiving file control
                           [else (data-control filenumber control-type)])
                         (send file-control-dialog show #f)))]))
    
    ; frame for when we want to view our chat history
    (define history-frame (new frame%
                               [label (format "~a (Chat History)" this-label)]
                               [width this-width]
                               [height this-height]))
    
    (define history-text (new text%
                              [line-spacing 1.0]
                              [auto-wrap #t]))
    (send history-text change-style black-style)
    
    (define history-canvas (new editor-canvas%
                                [parent history-frame]
                                [editor history-text]
                                [min-height this-height]
                                [min-width this-width]
                                [style (list 'control-border 'auto-hscroll
                                             'auto-vscroll)]))
    
    (define history-hpanel (new horizontal-panel%
                                [parent history-frame]
                                [alignment '(center center)]))
    
    (define history-close-button (new button%
                                      [label "Close"]
                                      [parent history-hpanel]
                                      [callback (λ (button event)
                                                  (send history-frame show #f)
                                                  (send history-text do-edit-operation
                                                        'select-all)
                                                  (send history-text do-edit-operation
                                                        'clear))]))
    
    ; view friend chat history
    (new menu-item% [parent menu-file]
         [label "Chat History"]
         [help-string "View chat history for this friend"]
         [callback (λ (button event)
                     (let-values ([(message timestamp who) (get-history my-id-hex friend-key)])
                       (for-each
                        (λ (x y z)
                          (define name "")
                          (cond [(zero? y) (set! name fname)]
                                [else (set! name "Me")])
                          (send history-text insert (format "[~a] ~a: ~a\n" x name z)))
                        timestamp who message)
                       (send history-frame show #t)))])
    
    ; close the current window
    (new menu-item% [parent menu-file]
         [label "&Close"]
         [help-string "Close Window"]
         [callback (λ (button event)
                     (send this show #f))])
    
    (define avatar-view-frame (new frame%
                                   [label "Blight - Avatar View"]))
    
    (define avatar-view-canvas
      (new canvas%
           [parent avatar-view-frame]
           [min-width avatar-width]
           [min-height avatar-height]
           [paint-callback
            (λ (l e)
              (let ([dc (send l get-dc)]
                    [avatar-big friend-avatar])
                (send dc draw-bitmap avatar-big 0 0)))]))
    
    (define chat-frame-hpanel (new horizontal-panel%
                                   [parent chat-frame]
                                   [stretchable-height #f]))
    
    (define friend-avatar-button (new button%
                                      [parent chat-frame-hpanel]
                                      [style (list 'deleted)]
                                      [label friend-avatar]
                                      [callback (λ (button event)
                                                  (send avatar-view-frame show #t))]))

    (define chat-frame-vpanel (new vertical-panel%
                                   [parent chat-frame-hpanel]
                                   [horiz-margin 8]
                                   [alignment '(left center)]))
    
    ; make a static text message in the frame containing friend's name
    ; replaced immediately by update-list-box
    (define chat-frame-msg (new message%
                                [parent chat-frame-vpanel]
                                [label this-label]
                                [min-width 40]
                                [auto-resize #t]))
    
    ; secondary frame message containing friend's status
    ; replaced by update-list-box
    (define chat-frame-status-msg (new message%
                                       [parent chat-frame-vpanel]
                                       [label ""]
                                       [auto-resize #t]))
    
    #;(define etext%
      (class text%
        (super-new)
        (define/override (on-default-event event)
          (let ([evt (send event get-event-type)]
                [editor this]
                [x-mouse (send event get-x)]
                [y-mouse (send event get-y)])
            (cond [(eq? evt 'right-up)
                   ; open the right-click menu
                   (let* ([ecanvas (send editor get-canvas)])
                     
                     (define popup
                       (new popup-menu% [title "Right Click Menu"]))
                     
                     (define copy-item
                       (new menu-item%
                            [label "Copy"]
                            [parent popup]
                            [help-string "Copy this selection"]
                            [callback (λ (l e)
                                        (send editor copy))]))
                     
                     (send ecanvas popup-menu popup x-mouse y-mouse))]
                  [(eq? evt 'left-down)
                   (send editor set-position x-mouse y-mouse)])))))
    
    (define chat-text-receive (new text%
                                   [line-spacing 1.0]
                                   [auto-wrap #t]))
    (send chat-text-receive change-style black-style)
    
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
        
        (define/override (on-char key-event)
          (let ([key (send key-event get-key-code)])
            (cond [(or (eq? key #\backspace)
                       (eq? key #\rubout)
                       (eq? key #\return))
                   (set-self-typing! this-tox friend-num #f)
                   (send editor-keymap handle-key-event this-editor key-event)]
                  [else
                   (when (and (not (send editor-keymap handle-key-event this-editor key-event))
                              (char? key))
                     (send this-editor insert key)
                     (set-self-typing! this-tox friend-num #t))])))

        (super-new
         [parent this-parent]
         [label this-label]
         [editor this-editor]
         [style this-style]
         [wheel-step this-wheel-step]
         [min-height this-min-height]
         [vert-margin this-vert-margin]
         [stretchable-height #f])))
    
    (define chat-editor-canvas-receive (new custom-receive-canvas%
                                            [parent chat-frame]
                                            [label "Messages received"]
                                            [editor chat-text-receive]
                                            [min-height (- this-height 22)] ; 358 exact height of buddy list
                                            [vert-margin 5]
                                            [style (list 'control-border 'no-hscroll
                                                         'auto-vscroll)]
                                            [wheel-step 3]
                                            [this-chat-window this]))

    (define message-history (new message-history%
                                 [editor chat-text-receive]))

    (define/public set-editor-black-style
      (λ (editor)
         (send editor change-style black-style)))
    
    (define chat-text-send (new text%
                                [line-spacing 1.0]
                                [auto-wrap #t]))
    (send chat-text-send change-style black-style)

    ; an editor canvas where text% messages will appear
    (define chat-editor-canvas-send (new custom-editor-canvas%
                                         [this-parent chat-frame]
                                         [this-label "Your message goes here"]
                                         [this-editor chat-text-send]
                                         [this-style (list 'control-border 'no-hscroll
                                                           'auto-vscroll)]
                                         [this-wheel-step 3]
                                         [this-min-height 100]
                                         [this-vert-margin 5]
                                         [this-chat-window this]))

    
    (define cur-focused-wdg chat-editor-canvas-send)
    
    (define/public cycle-focus
      (λ (forward)
        (cond 
          [(eq? cur-focused-wdg chat-editor-canvas-receive)
           (set! cur-focused-wdg chat-editor-canvas-send)]
          [else (set! cur-focused-wdg chat-editor-canvas-receive)])
        (send cur-focused-wdg focus)))
    
    
    
    (define panel (new horizontal-panel%
                       [parent chat-frame]
                       [stretchable-height #f]
                       [alignment '(right center)]))
    
    (define typing-msg (new message%
                            [parent panel]
                            [label ""]
                            [enabled #f]
                            [auto-resize #t]))
    
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
          (send chat-text-send insert (list-ref emojis i))
          (send emoji-dialog show #f))))
    
    ; second row of emoji
    (define row-two (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ([i (in-range 6)])
      (make-object button%
        (format "~a" (list-ref emojis (+ i 6)))
        row-two
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 6)))
          (send emoji-dialog show #f))))
    
    ; third row of emoji
    (define row-three (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ([i (in-range 6)])
      (make-object button%
        (format "~a" (list-ref emojis (+ i 12)))
        row-three
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 12)))
          (send emoji-dialog show #f))))
    
    ; fourth row of emoji
    (define row-four (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 18)))
        row-four
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 18)))
          (send emoji-dialog show #f))))
    
    ; fifth row of emoji
    (define row-five (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 24)))
        row-five
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 24)))
          (send emoji-dialog show #f))))
    
    ; sixth row of emoji
    (define row-six (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 30)))
        row-six
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 30)))
          (send emoji-dialog show #f))))
    
    ; seventh row of emoji
    (define row-seven (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 36)))
        row-seven
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 36)))
          (send emoji-dialog show #f))))
    
    ; eighth row of emoji
    (define row-eight (new horizontal-panel%
                           [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 42)))
        row-eight
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 42)))
          (send emoji-dialog show #f))))
    
    ; ninth row of emoji
    (define row-nine (new horizontal-panel%
                          [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 48)))
        row-nine
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 48)))
          (send emoji-dialog show #f))))
    
    ; tenth and final row of emoji
    (define row-ten (new horizontal-panel%
                         [parent emoji-dialog]))
    
    (for ((i (in-range 6)))
      (make-object button%
        (format "~a" (list-ref emojis (+ i 54)))
        row-ten
        (λ (button event)
          (send chat-text-send insert (list-ref emojis (+ i 54)))
          (send emoji-dialog show #f))))
    
    (new button%
         [parent emoji-dialog]
         [label "Close"]
         [callback (λ (button event)
                     (send emoji-dialog show #f))])
    #| #################### END EMOJI STUFF #################### |#
    
    ; send the message through tox and then add to history
    ; message is a string
    (define/public do-send-message
      (λ (editor message)
        ; add message to message history and get its type
        (define msg-type
          (send message-history add-send-message message (get-time)))
        
        (define do-send
          (λ (byte-str)
            (cond
              ; we're sending an action!
              [(eq? msg-type 'action)
               ; "/me " -> 4 bytes
               (friend-send-message this-tox friend-num
                                    'action (subbytes byte-str 4))]
              ; we're not doing anything special
              [else (friend-send-message this-tox friend-num
                                         'normal byte-str)])))
        
        ; split the message if it exceeds TOX_MAX_MESSAGE_LENGTH
        ; otherwise, just send it.
        (define split-message
          (λ (mbytes)
            (let ([len (bytes-length mbytes)])
              (cond [(<= len TOX_MAX_MESSAGE_LENGTH)
                     (do-send mbytes)]
                    [(> len TOX_MAX_MESSAGE_LENGTH)
                     (do-send (subbytes mbytes 0 TOX_MAX_MESSAGE_LENGTH))
                     (split-message (subbytes mbytes TOX_MAX_MESSAGE_LENGTH))]))))
        
        (split-message (string->bytes/utf-8 message))
        ; add messages to history
        (add-history my-id-hex friend-key (send editor get-text) 1)))
    
    
    ; guess I need to override some shit to get the keys just right
    
    (define editor-keymap (init-editor-keymap this))
    
    (set-default-editor-bindings editor-keymap)
    (send editor-keymap chain-to-keymap chatframe-keymap #t)
    
    (send chat-editor-canvas-send focus)
    
    (define/public (get-tox)
      this-tox)
    
    (define/public (set-new-label x)
      (send chat-frame set-label x))
    
    (define/override (show x)
      (send chat-frame show x))
    
    (define/override (is-shown?)
      (send chat-frame is-shown?))
    
    (define/override (is-enabled?)
      (send chat-frame is-enabled?))
    
    (define/public (set-name name)
      (set! fname name)
      (send chat-frame-msg set-label name)
      (send typing-msg set-label (string-append name " is not typing ")))
    
    (define/public (get-name)
      fname)
    
    (define/public (get-receive-editor)
      (send chat-editor-canvas-receive get-editor))

    (define/public (get-msg-history)
      message-history)
    
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
    
    (define/public (close-transfer id)
      (when (hash-has-key? transfers id)
        (transfers-del! id)))
    
    (define/public (set-status-msg msg)
      ; check the title for &'s and "escape" them
      (send chat-frame-status-msg set-label (string-replace msg "&" "&&")))
    
    (define/public (get-status-msg)
      (send chat-frame-status-msg get-label))
    
    (define/public (set-friend-avatar avatar)
      (cond [(path? avatar)
             ; create the bitmap
             (define avatar-bitmap (make-bitmap 40 40))
             ; load the file into the bitmap
             (send avatar-bitmap load-file avatar)
             ; turn it into a pict
             (define avatar-pict (bitmap avatar-bitmap))
             ; scale the pict to 40x40
             (define avatar-pict-small (scale-to-fit avatar-pict 40 40))
             ; add avatar button to panel
             (when (not friend-has-avatar?)
               (send chat-frame-hpanel add-child friend-avatar-button)
               (set! friend-has-avatar? #t))

             ; set the avatar to the new one
             (set! friend-avatar avatar-bitmap)
             ; set the button to the scaled avatar
             (send friend-avatar-button set-label (pict->bitmap avatar-pict-small))]
            [else (send friend-avatar-button set-label (make-bitmap 40 40))]))
    
    (define/public (get-friend-avatar)
      friend-avatar)
    
    (define/public (get-fc-lb)
      fc-list-box)
    
    (define/public (is-typing? bool)
      (if bool
          (send typing-msg set-label
                (string-append fname " is typing... "))
          (send typing-msg set-label
                (string-append fname " is not typing ")))
      (send typing-msg enable bool))
    
    (define/public (get-typing-msg)
      typing-msg)
    
    ; for read-receipt stuff
    (define/public (set-msg-unread)
      (set-new-label (string-append "Blight - " fname " *"))
      (set! msg-read? false))
    
    (define/public (get-msg-read)
      msg-read?)
    
    (define/public (window-has-focus?)
      (send chat-frame has-focus?))
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
