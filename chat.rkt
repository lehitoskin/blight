#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket
         racket/flonum
         "helpers.rkt"
         "number-conversions.rkt"
         "history.rkt"
         "config.rkt"
         "msg-editor.rkt"
         "msg-history.rkt"
         "utils.rkt"
         )
(provide (all-defined-out)
         fl->exact-integer)

#|
 # issues:
 # - need to reimplement a whole bunch of key events because we're
 #   overriding this, baby!
 |#

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

; TODO: make tail-recursive (start at (bytes-length bstr) and end at 0)
(define hex-string->bytes
  (λ (hexstr len)
    (define bstr #"")
    (do ((i 0 (+ i 1))
         (j 0 (+ j 2)))
      ((= i len))
      (set! bstr (bytes-append
                  bstr
                  (bytes
                   (hex->dec
                    (string-append
                     (string (string-ref hexstr j))
                     (string (string-ref hexstr (+ j 1)))))))))
    bstr))

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
                this-tox)
    
    (set-default-chatframe-bindings chatframe-keymap)
    
    (define friend-name "")
    (define friend-key "")
    (define friend-num -1)
    ; the sending file transfer list and its path list
    ; easier to have two lists than deal with a list of pairs
    (define stransfers null)
    (define paths null)
    
    ;; (define/private repeat
    ;;   (λ (proc times)
    ;;     (cond [(zero? times) #t]
    ;;           [else (proc) (repeat proc (- times 1))])))
    
    (define add-file-sender
      (λ (path filenumber)
        (define filename (path->string path))
        (send transfer-gauge set-value 0)
        (set! stransfers (append stransfers (list (file->bytes path))))))
    
    (define/public send-data
      (λ (filenumber)
        (define path (list-ref paths filenumber))
        
        (send message-history
                   begin-send-file path (get-time))
        
        (define size (file-size path))
        (define sent 0)
        (define percent 0)
        ; maximum piece size we can send at one time
        (define max-size (file-data-size this-tox friend-num))
        ; number of pieces we're going to send
        (define num-pieces (quotient size max-size))
        (add-file-sender path filenumber)
        (do ((i 0 (+ i 1)))
          ((= i num-pieces))
          (let ((piece (subbytes (list-ref stransfers filenumber)
                                 (* max-size i) (* max-size (+ i 1)))))
            ; send our piece
            ; if there is an error, sleep and then try again.
            (let loop ()
              (cond [(= -1 (send-file-data this-tox friend-num
                                           filenumber piece (bytes-length piece)))
                     (tox-do this-tox)
                     (sleep (/ (tox-do-interval this-tox) 1000))
                     (loop)]))
            ; update file-send gauge
            (set! sent (+ sent (bytes-length piece)))
            (set! percent (fl->exact-integer (truncate (* (exact->inexact (/ sent size)) 100))))
            (send transfer-gauge set-value percent)))
        ; if there is a remainder, send the very last piece
        (unless (zero? (quotient size max-size))
          (let ((piece (subbytes (list-ref stransfers filenumber)
                                 (- size (remainder size max-size)) size)))
            ; send our piece
            ; if there is an error, sleep and then try again.
            (let loop ()
              (cond [(= -1 (send-file-data this-tox friend-num
                                           filenumber piece (bytes-length piece)))
                     (tox-do this-tox)
                     (sleep (/ (tox-do-interval this-tox) 1000))
                     (loop)]))
            ; update file-send gauge
            (set! sent (+ sent (bytes-length piece)))
            (set! percent (fl->exact-integer (truncate (* (exact->inexact (/ sent size)) 100))))
            (send transfer-gauge set-value percent)))
        ; tell our friend we're done sending
        (send-file-control this-tox friend-num
                           #f filenumber
                           (_TOX_FILECONTROL-index 'FINISHED)
                           #f 0)

             (send message-history
                   end-send-file path (get-time))

             (unless (false? make-noise)
                 (play-sound (eighth sounds) #t))))
    
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
    
    ; send a file to our friend
    (new menu-item% [parent menu-file]
         [label "Send File"]
         [help-string "Send a file to this friend"]
         [callback (λ (button event)
                     ; create a new thread so we don't get disconnected
                     (thread
                      (λ ()
                        (let ((path (get-file "Select a file to send")))
                          (unless (false? path)
                            ; total size of the file
                            (define size (file-size path))
                            ; name of the file (taken from the path)
                            (define filename (path->string (last (explode-path path))))
                            (define filenumber
                              (new-file-sender this-tox friend-num size filename))
                            (set! paths (append paths (list path))))))))])
    
    ; close the current window
    (new menu-item% [parent menu-file]
         [label "&Close"]
         [help-string "Close Window"]
         [callback (λ (button event)
                     (send this show #f))])
    
    ; make a static text message in the frame
    ; replaced immediately by update-list-box
    (define chat-frame-msg (new message%
                                [parent chat-frame]
                                [label this-label]
                                [min-width 40]))
    (send chat-frame-msg auto-resize #t)
    
    ; secondary frame message containing friend's status
    ; replaced by update-list-box
    (define chat-frame-status-msg (new message%
                                       [parent chat-frame]
                                       [label ""]
                                       [auto-resize #t]))
    
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
    
    (define chat-editor-canvas-receive (new custom-receive-canvas%
                                            [parent chat-frame]
                                            [label "Messages received"]
                                            [editor chat-text-receive]
                                            [min-height 400]
                                            [vert-margin 5]
                                            [style (list 'control-border 'no-hscroll
                                                         'auto-vscroll)]
                                            [wheel-step 3]
                                            [this-chat-window this]))

    (define message-history (new message-history%
                                 [editor chat-text-receive]))

    (define/public set-editor-black-style
      (λ (editor)
         (send editor change-style black-style)
         ))
    
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
    
    ; send the message to chat-text-receive and then through tox
    ; assumes msg is already a byte-string. the editor parameter refers to chat-text-send
    (define/public do-send-message
      (λ (editor msg-bytes)
        ; procedure to send to the editor and to tox
        (define do-send
          (λ (byte-str)
            ; if the current cursor position is not at the end, move there
            (cond [(not (= (send chat-text-receive get-start-position)
                           (send chat-text-receive get-end-position)))
                   (send chat-text-receive move-position 'end)
                   ; parse for /commands, >implying
                   (cond
                     ; we're >implying something
                     [(bytes=? (subbytes byte-str 0 1) #">")
                      (send chat-text-receive insert
                            (string-append "[" (get-time) "] Me: "))
                      (imply chat-text-receive (bytes->string/utf-8 byte-str))
                      (send-message this-tox friend-num byte-str (bytes-length byte-str))]
                     ; we're sending an action!
                     [(and (> (bytes-length byte-str) 4)
                           (bytes=? (subbytes byte-str 0 4) #"/me "))
                      (send chat-text-receive insert
                            (string-append "** [" (get-time) "] Me "))
                      (send chat-text-receive insert
                            (bytes->string/utf-8 (bytes-append
                                                  (subbytes byte-str 4) #"\n")))
                      (send-action this-tox friend-num
                                   (subbytes byte-str 4) (bytes-length byte-str))]
                     ; we're not doing anything special
                     [else (send chat-text-receive insert
                                 (string-append "[" (get-time) "] Me: "))
                           (send chat-text-receive insert
                                 (bytes->string/utf-8 (bytes-append byte-str #"\n")))
                           (send-message this-tox friend-num byte-str
                                         (bytes-length byte-str))])]
                  ; otherwise just insert the message
                  [(= (send chat-text-receive get-start-position)
                      (send chat-text-receive get-end-position))
                   ; parse for /commands, >greentext
                   (cond
                     ; we're >implying something
                     [(bytes=? (subbytes byte-str 0 1) #">")
                      (send chat-text-receive insert
                            (string-append "[" (get-time) "] Me: "))
                      (imply chat-text-receive (bytes->string/utf-8 byte-str))
                      (send-message this-tox friend-num byte-str (bytes-length byte-str))]
                     ; we're sending an action!
                     [(and (> (bytes-length byte-str) 4)
                           (bytes=? (subbytes byte-str 0 4) #"/me "))
                      (send chat-text-receive insert
                            (string-append "** [" (get-time) "] Me "))
                      (send chat-text-receive insert
                            (bytes->string/utf-8 (bytes-append
                                                  (subbytes byte-str 4) #"\n")))
                      (send-action this-tox friend-num
                                   (subbytes byte-str 4) (bytes-length byte-str))]
                     ; we're not doing anything special
                     [else (send chat-text-receive insert
                                 (string-append "[" (get-time) "] Me: "))
                           (send chat-text-receive insert
                                 (bytes->string/utf-8 (bytes-append byte-str #"\n")))
                           (send-message this-tox friend-num byte-str
                                         (bytes-length byte-str))])])))
        (cond [(> (bytes-length msg-bytes) (* TOX_MAX_MESSAGE_LENGTH 2))
               ; if the message is greater than twice our max length, split it
               ; into three chunks
               (define-values (first-third second-third third-third)
                 (split-bytes msg-bytes))
               ; send first third
               (do-send first-third)
               (do-send second-third)
               (do-send third-third)
               ; add messages to history
               ; obtain our tox id
               (define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
               (get-address this-tox my-id-bytes)
               (define my-id-hex
                 (bytes->hex-string my-id-bytes))
               (add-history my-id-hex friend-key (send editor get-text) 1)]
              [(and (> (bytes-length msg-bytes) TOX_MAX_MESSAGE_LENGTH)
                    (<= (bytes-length msg-bytes) (* TOX_MAX_MESSAGE_LENGTH 2)))
               ; if the message is greater than our max length, but less than
               ; or equal to twice the max length, split it into two chunks
               (define-values (first-half second-half) (split-bytes msg-bytes))
               (do-send first-half)
               (do-send second-half)
               ; add messages to history
               ; obtain our tox id
               (define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
               (get-address this-tox my-id-bytes)
               (define my-id-hex
                 (bytes->hex-string my-id-bytes))
               (add-history my-id-hex friend-key (send editor get-text) 1)]
              [else
               (do-send msg-bytes)
               ; add message to history
               ; obtain our tox id
               (define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
               (get-address this-tox my-id-bytes)
               (define my-id-hex
                 (bytes->hex-string my-id-bytes))
               (add-history my-id-hex friend-key (send editor get-text) 1)])))
    
    
    ; guess I need to override some shit to get the keys just right
    
    (define editor-keymap (init-editor-keymap this))
    
    (set-default-editor-bindings editor-keymap)
    (send editor-keymap chain-to-keymap chatframe-keymap #t)
    
    (send chat-editor-canvas-send focus)
    
    (define transfer-gauge (new gauge%
                                [label "Transfers "]
                                [parent chat-frame]
                                [range 100])) ; range in percentage
    
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
      (set! stransfers (delnode stransfers filenumber))
      (set! paths (delnode paths filenumber)))
    
    (define/public (set-gauge-pos num)
      (send transfer-gauge set-value num))
    
    (define/public (set-status-msg msg)
      (send chat-frame-status-msg set-label msg))
    
    (define/public (get-status-msg)
      (send chat-frame-status-msg get-label))
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
