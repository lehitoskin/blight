#lang racket/gui
; chat.rkt
; contains chat-window definitions
(require libtoxcore-racket
         racket/flonum
         "helpers.rkt"
         "number-conversions.rkt"
         "history.rkt"
         "config.rkt")
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

(define font-size-delta
  (make-object style-delta% 'change-size 10))

; TODO: make tail-recursive (start at (bytes-length bstr) and end at 0)
(define bytes->hex-string
  (λ (bstr len)
    (define hex "")
    (do ((i 0 (+ i 1)))
      ((= i (bytes-length bstr)))
      (set! hex (string-append
                 hex
                 (string-upcase
                  (dec->hex (bytes-ref bstr i))))))
    hex))

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
                    [(is-a? wdg text%) (send (send wdg get-canvas) get-parent)] ; key event from editor% 
                    [(subclass? wdg frame%) wdg])]) ; key event from frame itself
              (send chatframe show #f))))
    km))

(define chatframe-keymap (init-chatframe-keymap))

(define (set-default-chatframe-bindings km)
  (send km map-function ":c:w" "close-chatframe")
  (send km map-function ":c:ц" "close-chatframe") ; russian cyrillic
  )

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
    
    (define/private repeat
      (λ (proc times)
        (cond [(zero? times) #t]
              [else (proc) (repeat proc (- times 1))])))
    
    (define add-file-sender
      (λ (path filenumber)
        (define filename (path->string path))
        (send transfer-gauge set-value 0)
        (if (zero? filenumber)
            ; our first sending transfer, replace the null
            (set! stransfers (setnode stransfers (file->bytes path) filenumber))
            ; not our first, append to the list
            (set! stransfers (append stransfers (list (file->bytes path)))))))
    
    (define/public send-data
      (λ (filenumber)
        (define path (list-ref paths filenumber))
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
        (send chat-text-receive insert "\n***FILE TRANSFER COMPLETED***\n\n")))
    
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
                            (if (zero? filenumber)
                                ; our first sending transfer, replace the null
                                (set! paths (setnode paths path filenumber))
                                ; not our first, append to the list
                                (set! paths (append paths (list path)))))))))])
    
    ; close the current window
    (new menu-item% [parent menu-file]
         [label "&Close"]
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
                    vert-margin)
        
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
    
    (define chat-editor-canvas-receive (new custom-receive-canvas%
                                            [parent chat-frame]
                                            [label "Messages received"]
                                            [editor chat-text-receive]
                                            [min-height 400]
                                            [vert-margin 5]
                                            [style (list 'control-border 'no-hscroll
                                                         'auto-vscroll)]
                                            [wheel-step 3]))
    
    (define panel (new horizontal-panel%
                       [parent chat-frame]
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
    
    ; send the message to the editor and then through tox
    ; assumes msg is already a byte-string
    (define do-send-message
      (λ (editor msg-bytes)
        ; procedure to send to the editor and to tox
        (define do-send
          (λ (byte-str)
            (send chat-text-receive insert
                  (string-append "[" (get-time) "] Me: " (bytes->string/utf-8 byte-str) "\n"))
            (send-message this-tox friend-num msg-bytes (bytes-length byte-str))))
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
                 (bytes->hex-string my-id-bytes TOX_FRIEND_ADDRESS_SIZE))
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
                 (bytes->hex-string my-id-bytes TOX_FRIEND_ADDRESS_SIZE))
               (add-history my-id-hex friend-key (send editor get-text) 1)]
              [else
               (do-send msg-bytes)
               ; add message to history
               ; obtain our tox id
               (define my-id-bytes (make-bytes TOX_FRIEND_ADDRESS_SIZE))
               (get-address this-tox my-id-bytes)
               (define my-id-hex
                 (bytes->hex-string my-id-bytes TOX_FRIEND_ADDRESS_SIZE))
               (add-history my-id-hex friend-key (send editor get-text) 1)])))
    
    (define chat-text-send (new text%
                                [line-spacing 1.0]
                                [auto-wrap #t]))
    (send chat-text-send change-style font-size-delta)
    
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
                    (send editor change-style font-size-delta)))))

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
      (send km map-function ":c:4" "special-insert-symbol"))

(set-default-editor-bindings editor-keymap)
(send editor-keymap chain-to-keymap chatframe-keymap #t)

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
    
    (super-new
     [label this-label]
     [height this-height]
     [width this-width])))
