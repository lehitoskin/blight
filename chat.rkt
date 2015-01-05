#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket
         (only-in racket/flonum
                  fl->exact-integer)
         "helpers.rkt"
         "number-conversions.rkt"
         "history.rkt"
         "config.rkt"
         "msg-editor.rkt"
         "msg-history.rkt"
         "utils.rkt"
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

(define bytes->hex-string
  (λ (bstr)
    (define blist (bytes->list bstr))
    (define stuff (λ (item)
                    (string->list (string-upcase (dec->hex item)))))
    (list->string (flatten (map stuff blist)))))

; recursion! whee!
(define hex-string->bytes
  (λ (hexstr len)
    (cond [(zero? len) #""]
          [else
           (bytes-append
            (bytes
             (hex->dec
              (substring hexstr 0 2)))
            (hex-string->bytes (substring hexstr 2) (- len 1)))])))

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
    
    (define friend-name "")
    (define friend-key "")
    (define friend-num -1)
    ; obtain our tox id
    (define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
    (get-address this-tox my-id-bytes)
    (define my-id-hex (bytes->hex-string my-id-bytes))
    (define friend-avatar (make-bitmap 40 40))
    
    ; the sending file transfer list and its path list
    ; easier to have two lists than deal with a list of pairs
    
    ;; (define/private repeat
    ;;   (λ (proc times)
    ;;     (cond [(zero? times) #t]
    ;;           [else (proc) (repeat proc (- times 1))])))
    
    (define add-file-sender
      (λ (path filenumber)
        (define filename (path->string path))
        (send transfer-gauge set-value 0)
	(st-read-file! filenumber)))
    
    (define data-control
      (λ (filenumber sending? type)
        (send-file-control this-tox friend-num
                           sending? filenumber
                           (_TOX_FILECONTROL type)
                           #f 0)))
    
    (define/public send-data
      (λ (filenumber)
        (define path (st-ref-path filenumber))
        
        (send message-history
              begin-send-file path (get-time))
        
        (define size (file-size path))
        (define percent 0)
        ; maximum piece size we can send at one time
        (define max-size (file-data-size this-tox friend-num))
        ; number of pieces we're going to send
        (define num-pieces (quotient size max-size))
        (add-file-sender path filenumber)
        (do ((i 0 (+ i 1)))
          ((= i num-pieces))
          (let ([piece (subbytes (st-ref-data filenumber)
                                 (* max-size i) (* max-size (+ i 1)))])
            ; send our piece
            ; if there is an error, sleep and then try again.
            (let loop ()
              (cond [(= -1 (send-file-data this-tox friend-num
                                           filenumber piece (bytes-length piece)))
                     (tox-do this-tox)
                     (sleep (/ (tox-do-interval this-tox) 1000))
                     (loop)]))
            ; update file-send gauge
            (set-st-sent! filenumber
                          (+ (st-ref-sent filenumber) (bytes-length piece)))
            (set! percent (fl->exact-integer
                           (truncate (* (exact->inexact
                                         (/ (st-ref-sent filenumber) size)) 100))))
            (send transfer-gauge set-value percent)))
        ; if there is a remainder, send the very last piece
        (unless (zero? (quotient size max-size))
          (let ([piece (subbytes (st-ref-data filenumber)
                                 (- size (remainder size max-size)) size)])
            ; send our piece
            ; if there is an error, sleep and then try again.
            (let loop ()
              (cond [(= -1 (send-file-data this-tox friend-num
                                           filenumber piece (bytes-length piece)))
                     (tox-do this-tox)
                     (sleep (/ (tox-do-interval this-tox) 1000))
                     (loop)]))
            ; update file-send gauge
            (set-st-sent! filenumber (+ (st-ref-sent filenumber) (bytes-length piece)))
            (set! percent (fl->exact-integer (truncate
                                              (* (exact->inexact
                                                  (/ (st-ref-sent filenumber) size)) 100))))
            (send transfer-gauge set-value percent)))
        ; tell our friend we're done sending
        (data-control filenumber #f 'FINISHED)
        
        (send message-history
              end-send-file path (get-time))
        
        (unless (false? make-noise)
          (play-sound (eighth sounds) #t))))
    
    (define/public resume-data
      (λ (filenumber)
        (let* ([path (st-ref-path filenumber)]
               [size (file-size path)]
               [max-size (file-data-size this-tox friend-num)]
               [num-pieces (quotient size max-size)]
               [num-left (- num-pieces (st-ref-sent filenumber))]
               [percent (fl->exact-integer (truncate
                                            (* (exact->inexact
                                                (/ (st-ref-sent filenumber) size)) 100)))])
          ; send the data!
          (do ((i 0 (+ i 1)))
            ((= i num-left))
            (let ([piece (subbytes (st-ref-data filenumber)
                                   (* max-size i) (* max-size (+ i 1)))])
              (let loop ()
                ; if there was an error, try again!
                (cond [(= -1 (send-file-data this-tox friend-num
                                             filenumber piece (bytes-length piece)))
                       (tox-do this-tox)
                       (sleep (/ (tox-do-interval this-tox) 1000))
                       (loop)]))
              (set-st-sent! filenumber (+ (st-ref-sent filenumber) (bytes-length piece)))
              (set! percent (fl->exact-integer (truncate
                                                (* (exact->inexact
                                                    (/ (st-ref-sent filenumber) size)) 100))))
              (send transfer-gauge set-value percent)))
          ; if there's a remainder, send that last bit
          (unless (zero? (quotient size max-size))
            (let ([piece (subbytes (st-ref-data filenumber)
                                   (- size (remainder size max-size)) size)])
              ; if there was an error, try again
              (let loop ()
                (cond [(= -1 (send-file-data this-tox friend-num
                                             filenumber piece (bytes-length piece)))
                       (tox-do this-tox)
                       (sleep (/ (tox-do-interval this-tox) 1000))
                       (loop)]))
              (set-st-sent! filenumber (+ (st-ref-sent filenumber) (bytes-length piece)))
              (set! percent (fl->exact-integer (truncate
                                                (* (exact->inexact
                                                    (/ (st-ref-sent filenumber) size)) 100))))
              (send transfer-gauge set-value percent)))
          ; tell our friend we're done sending
          (data-control filenumber #f 'FINISHED)
          (send message-history
                end-send-file path (get-time))
          (unless (false? make-noise)
            (play-sound (eighth sounds) #t)))))
    
    ; create a new top-level window
    ; make a frame by instantiating the frame% class
    (define chat-frame (new frame%
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
    (new menu-item% [parent menu-file]
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
                            (define filename (path->string (last (explode-path path))))
                            (define filenumber
                              (new-file-sender this-tox friend-num size filename))
                            (st-add! path filenumber))))))])
    
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
    
    (define fc-tab-panel
      (new tab-panel%
           [parent file-control-dialog]
           [choices '("Receiving"
                      "Sending")]
           [callback (λ (l e)
                       (cond [(zero? (send l get-selection))
                              (send l delete-child fc-sending-hpanel)
                              (send l add-child fc-receiving-hpanel)]
                             [else (send l delete-child fc-receiving-hpanel)
                                   (send l add-child fc-sending-hpanel)]))]))
    
    (define fc-receiving-hpanel
      (new horizontal-panel%
           [parent fc-tab-panel]))
    
    (define fc-sending-hpanel
      (new horizontal-panel%
           [parent fc-tab-panel]
           [style '(deleted)]))
    
    (define fc-receiving-list-box
      (new list-box%
           [parent fc-receiving-hpanel]
           [label "Files Available for Control"]
           [style '(single vertical-label)]
           [choices (list "")]))
    
    (define fc-receiving-rbox
      (new radio-box%
           [parent fc-receiving-hpanel]
           [label #f]
           [choices (list "Pause"
                          "Kill"
                          "Resume_Broken")]))
    
    (define fc-sending-list-box
      (new list-box%
           [parent fc-sending-hpanel]
           [label "Files Available for Control"]
           [style '(single vertical-label)]
           [choices (list "")]))
    
    (define fc-sending-rbox
      (new radio-box%
           [parent fc-sending-hpanel]
           [label #f]
           [choices (list "Pause"
                          "Kill"
                          "Resume_Broken")]))
    
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
                       (let* ([sel (send fc-tab-panel get-selection)]
                              [fc-lb (if (zero? sel)
                                         fc-receiving-list-box
                                         fc-sending-list-box)]
                              [fc-rb (if (zero? sel)
                                         fc-receiving-rbox
                                         fc-sending-rbox)]
                              [filenumber (send fc-lb get-selection)]
                              [control-type (string->symbol
                                             (string-upcase
                                              (send fc-rb get-item-label
                                                    (send fc-rb get-selection))))])
                         (cond
                           ; no file transfers going on, do nothing
                           [(and (hash-empty? rt) (hash-empty? st))]
                           ; receiving file control
                           [(zero? sel) (data-control filenumber #t control-type)]
                           ; sending file control
                           [(= sel 1) (data-control filenumber #f control-type)])
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
                          (cond [(zero? y) (set! name friend-name)]
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
                                   [parent chat-frame]))
    
    (define friend-avatar-button (new button%
                                      [parent chat-frame-hpanel]
                                      [label friend-avatar]
                                      [callback (λ (button event)
                                                  (send avatar-view-frame show #t))]))
    
    (define chat-frame-vpanel (new vertical-panel%
                                   [parent chat-frame-hpanel]
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

        ; TODO:
        ; unicode?
        ; wheel-up/wheel-down(?)
        (define/override (on-char key-event)
          (let ([key (send key-event get-key-code)])
            (if (or (eq? key #\backspace)
                    (eq? key #\rubout)
                    (eq? key #\return))
                (begin
                  (set-user-is-typing this-tox friend-num #f)
                  (send editor-keymap handle-key-event this-editor key-event))
                (when (not (send editor-keymap handle-key-event this-editor key-event))
                  (send this-editor insert key)
                  (set-user-is-typing this-tox friend-num #t)))))

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
                                            [min-height 358] ; exact height of buddy list
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
    (define/public do-send-message
      (λ (editor message)
        ; procedure to send to the editor and to tox

      ; add message to message history and get its type
      (define msg-type
        (send message-history add-send-message message (get-time)))

      (define msg-bytes (string->bytes/utf-8 message))      

        (define do-send
          (λ (byte-str)
            (cond
             ; we're sending an action!
             [(eq? msg-type 'action)
              (send-action this-tox friend-num
                           (subbytes byte-str 4) (bytes-length byte-str))]
             ; we're not doing anything special
             [else (send-message this-tox friend-num byte-str
                                 (bytes-length byte-str))])))
        
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
        
        (split-message msg-bytes)
        ; add messages to history
        (add-history my-id-hex friend-key (send editor get-text) 1)))
    
    
    ; guess I need to override some shit to get the keys just right
    
    (define editor-keymap (init-editor-keymap this))
    
    (set-default-editor-bindings editor-keymap)
    (send editor-keymap chain-to-keymap chatframe-keymap #t)
    
    (send chat-editor-canvas-send focus)
    
    (define transfer-gauge (new gauge%
                                [label "Transfers "]
                                [parent chat-frame]
                                [range 100])) ; range in percentage
    
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
      (set! friend-name name)
      (send chat-frame-msg set-label name)
      (send typing-msg set-label (string-append name " is typing... ")))
    
    (define/public (get-name)
      friend-name)
    
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
    
    (define/public (close-transfer filenumber)
      (st-del! filenumber))
    
    (define/public (set-gauge-pos num)
      (send transfer-gauge set-value num))
    
    (define/public (set-status-msg msg)
      (send chat-frame-status-msg set-label msg))
    
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
             ; set the avatar to the new one
             (set! friend-avatar avatar-bitmap)
             ; set the button to the scaled avatar
             (send friend-avatar-button set-label (pict->bitmap avatar-pict-small))]
            [else (send friend-avatar-button set-label (make-bitmap 40 40))]))
    
    (define/public (get-friend-avatar)
      friend-avatar)
    
    (define/public (get-fc-sending-lb)
      fc-sending-list-box)
    
    (define/public (get-fc-receiving-lb)
      fc-receiving-list-box)
    
    (define/public (is-typing? bool)
      (send typing-msg enable bool))
    
    (define/public (get-typing-msg)
      typing-msg)
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
