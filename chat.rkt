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
