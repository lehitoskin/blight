#lang racket/gui
; callbacks.rkt
(require libtoxcore-racket
         libopenal-racket
         ffi/unsafe
         "audio.rkt"
         "blight.rkt"
         "config.rkt"
         "helpers.rkt"
         "history.rkt"
         "tox.rkt"
         "utils.rkt"
         "gui/chat.rkt"
         "gui/frame.rkt"
         "gui/friend-list.rkt"
         "gui/msg-history.rkt"
         "gui/smart-list.rkt")

(provide on-status-type-change)

#| ################# START CALLBACK PROCEDURE DEFINITIONS ################# |#
; set all the callback functions
(define on-friend-request
  (λ (mtox key-ptr message len userdata)
    (define public-key (make-sized-byte-string key-ptr TOX_PUBLIC_KEY_SIZE))
    (define add-pubkey-ptr (malloc TOX_PUBLIC_KEY_SIZE 'atomic))
    (for ([i (in-range (/ TOX_PUBLIC_KEY_SIZE 2))])
      (ptr-set! add-pubkey-ptr (* i 2) _byte (bytes-ref public-key i))
      (ptr-set! add-pubkey-ptr (add1 (* i 2)) _byte (bytes-ref public-key i)))
    ; convert public-key from bytes to string so we can display it
    (define id-hex (bytes->hex-string public-key))
    ; friend request dialog
    (define fr-dialog
      (new dialog%
           [label "Blight - Friend Request"]
           [style (list 'close-button)]))
    
    ; friend request text with modified text size
    (define fr-text
      (new text%
           [line-spacing 1.0]
           [auto-wrap #t]))
    (send fr-text change-style black-style)
    
    ; canvas to print the friend request message
    (define fr-ecanvas
      (new editor-canvas%
           [parent fr-dialog]
           [min-height 150]
           [min-width 650]
           [vert-margin 10]
           [editor fr-text]
           [style (list 'control-border 'no-hscroll
                        'auto-vscroll 'no-focus)]))
    
    ; panel to right-align our buttons
    (define fr-hpanel
      (new horizontal-panel%
           [parent fr-dialog]
           [alignment (list 'right 'center)]))
    
    (define fr-cancel-button
      (new button%
           [parent fr-hpanel]
           [label "Cancel"]
           [callback (λ (button event)
                       ; close and reset the friend request dialog
                       (send fr-dialog show #f))]))
    
    (define fr-ok-button
      (new button%
           [parent fr-hpanel]
           [label "OK"]
           [callback
            (λ (button event)
              ; add the friend
              (define friendnumber (add-friend-norequest mtox add-pubkey-ptr))
              (display "Adding friend... ")
              ; reused code to add friend on success
              (define (add-friend-success)
                ; play a sound because we accepted
                (when (make-noise)
                  (play-sound (sixth sounds) #f))
                (printf "Added friend number ~a~n" friendnumber)
                ; append new friend to the list
                (create-buddy (format-anonymous id-hex)
                              (friend-key my-tox friendnumber))
                
                ; update friend list
                ; add connection status icons to each friend
                (do ((i 0 (+ i 1)))
                  ((= i (friendlist-length mtox)))
                  (status-checker
                   i
                   (get-friend-connection-status mtox i)))
                ; the invite list needs to be updated for
                ; the groupchat windows that still exist
                (unless (zero? (hash-count cur-groups))
                  (update-invite-list))
                ; save the tox data
                (blight-save-data))
              ; catch errors
              (cond [(false? friendnumber)
                     (display "There was an error accepting the friend request! ")
                     ; if we've failed, try again 3(?) more times
                     (let loop ([tries 0])
                       (cond [(= tries 3)
                              (displayln "Failed!")
                              (when (make-noise)
                                (play-sound (last sounds) #t))]
                             [else
                              (display "Retrying... ")
                              (tox-do mtox)
                              (sleep (/ (tox-do-interval mtox) 1000))
                              (if (false? (add-friend-norequest mtox add-pubkey-ptr))
                                  (loop (add1 tries))
                                  (begin
                                    (displayln "Success!")
                                    (add-friend-success)))]))]
                    [else (add-friend-success)])
              (send fr-dialog show #f))]))
    
    (send fr-text insert (string-append
                          id-hex
                          "\nwould like to add you as a friend!\n"
                          "Message: " message))
    (send fr-dialog show #t)))

(define on-friend-message
  (λ (mtox friendnumber message len userdata)
     (let* ([window (get-contact-window friendnumber)]
           [msg-history (send window get-msg-history)]
           [name (send window get-name)])
      
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])
      (send msg-history add-recv-message (my-name) message name (get-time))
      
      ; make a noise
      (when (make-noise)
        (play-sound (first sounds) #t))
      ; add message to the history database
      (add-history (my-id-hex) (send window get-key) message 0))))

(define on-friend-action
  (λ (mtox friendnumber action len userdata)
    (let* ([window (get-contact-window friendnumber)]
           [msg-history (send window get-msg-history)]
           [name (send window get-name)])
      ; if the window isn't open, force it open
      (cond [(not (send window is-shown?)) (send window show #t)])

      (send msg-history add-recv-action action name (get-time))
      
      ; make a noise
      (when (make-noise)
        (play-sound (first sounds) #t))
      ; add message to the history database
      (add-history (my-id-hex) (send window get-key) (string-append "ACTION: " action) 0))))

(define on-friend-name-change
  (λ (mtox friendnumber newname len userdata)
     (let ([sn (get-contact-snip friendnumber)])
       (send sml rename-entry sn newname))

    (let ([window (get-contact-window friendnumber)])
      ; update the name in the list
      (send window set-name newname)
      ; update the name in the window
      (send window set-new-label (string-append "Blight - " newname))
      ; add connection status icon
      (status-checker friendnumber (get-friend-connection-status mtox friendnumber)))))


(define on-status-type-change
  (λ (mtox friendnumber status userdata)
    ; friend is online
    (cond [(= status (_TOX_USERSTATUS 'NONE))
           (send (get-contact-snip friendnumber) set-status 'available)
           (update-contact-status friendnumber 'available)]
          ; friend is away
          [(= status (_TOX_USERSTATUS 'AWAY))
           (send (get-contact-snip friendnumber) set-status 'away)
           (update-contact-status friendnumber 'away)]
          ; friend is busy
          [(= status (_TOX_USERSTATUS 'BUSY))
           (send (get-contact-snip friendnumber) set-status 'busy)
           (update-contact-status friendnumber 'busy)])))

(define on-connection-status-change
  (λ (mtox friendnumber status userdata)
    ; add a thingie that shows the friend is online
    (cond [(zero? status)
           (send (get-contact-snip friendnumber) set-status 'offline)
           (update-contact-status friendnumber 'offline)
           (when (make-noise)
             (play-sound (third sounds) #t))]
          [else
           (send (get-contact-snip friendnumber) set-status 'available)
           (update-contact-status friendnumber 'available)
           (when (make-noise)
             (play-sound (second sounds) #t))])))

; needs to be in its own thread, otherwise we'll d/c(?)
(define on-file-send-request
  (λ (mtox friendnumber filenumber filesize filename len userdata)
    (thread
     (λ ()
       (when (make-noise)
         (play-sound (seventh sounds) #t))
       (let* ([cd (get-contact-data friendnumber)]
              (mbox (message-box "Blight - File Send Request"
                                 (string-append
                                  (contact-data-name cd)
                                  " wants to send you "
                                  "\"" filename "\"")
                                 #f
                                 (list 'ok-cancel 'caution)))
              [window (contact-data-window cd)]
              
              [msg-history (send window get-msg-history)])
         (cond [(eq? mbox 'ok)
                
                (let ([path (put-file "Select a file"
                                      #f
                                      download-path
                                      filename)]
                      [window (get-contact-window friendnumber)])
                  (unless (false? path)
                    (define message-id (_TOX_FILECONTROL 'ACCEPT))
                    (define receive-editor
                      (send window get-receive-editor))
                    (send-file-control mtox friendnumber #t filenumber message-id #f 0)
                    (send window set-gauge-pos 0)
                    (rt-add! path filenumber)
                    (send msg-history
                          begin-recv-file path (get-time))))]))))))

(define on-file-control
  (λ (mtox friendnumber sending? filenumber control-type data-ptr len userdata)
    (let* ([window (get-contact-window friendnumber)]
           [receive-editor (send window get-receive-editor)]
           [fc-receiving-lb (send window get-fc-receiving-lb)]
           [fc-sending-lb (send window get-fc-sending-lb)]
           [msg-history (send window get-msg-history)]
           [update-fc-receiving (λ ()
                                  (send fc-receiving-lb set
                                        (sort (map (λ (x)
                                                     (number->string (car x)))
                                                   (hash->list rt))
                                              string<?)))]
           [update-fc-sending (λ ()
                                (send fc-sending-lb set
                                      (sort (map (λ (x)
                                                   (number->string (car x)))
                                                 (hash->list st))
                                            string<?)))])
      (with-handlers
          ([exn:blight:rtransfer?
            (lambda (ex)
              (blight-handle-exception ex)
              (send msg-history send-file-recv-error (exn-message ex)))])
        ; we've finished receiving the file
        (cond [(and (= control-type (_TOX_FILECONTROL 'FINISHED))
                    (false? sending?))
               (define data-bytes (make-sized-byte-string data-ptr len))
               (write-bytes data-bytes (rt-ref-fhandle filenumber))
               ; close receive transfer
               (close-output-port (rt-ref-fhandle filenumber))
               ; notify user transfer has completed
               (send msg-history
                     end-recv-file (get-time) (rt-ref-received filenumber))
               ; remove transfer from list
               (rt-del! filenumber)
               ; update file control receiving list box
               (update-fc-receiving)]
              ; cue that we're going to be sending the data now
              [(and (= control-type (_TOX_FILECONTROL 'ACCEPT)) sending?)
               ; update file control sending list box
               (update-fc-sending)
               (send window send-data filenumber)]
              [(= control-type (_TOX_FILECONTROL 'KILL))
               ; remove transfer from list
               (cond [sending?
                      (st-del! filenumber)
                      (update-fc-sending)]
                     [else
                      (close-output-port (rt-ref-fhandle filenumber))
                      (rt-del! filenumber)
                      (update-fc-receiving)])]
              ; resume sending file
              [(and (= control-type (_TOX_FILECONTROL 'RESUME_BROKEN)) sending?)
               (send window resume-data filenumber)]
              ; catch everything else and just update both of the list boxes
              [else
               (update-fc-receiving)
               (update-fc-sending)])))))

(define on-file-data
  (λ (mtox friendnumber filenumber data-ptr len userdata)
    
    (define data-bytes (make-sized-byte-string data-ptr len))
    (define window (get-contact-window friendnumber))
    (define msg-history (send window get-msg-history))
    
    (with-handlers
        ([exn:blight:rtransfer?
          (lambda (ex)
            (send msg-history send-file-recv-error (exn-message ex)))])
      (write-bytes data-bytes (rt-ref-fhandle filenumber))
      (set-rt-received! filenumber len)
      (send window set-gauge-pos
            (fl->exact-integer (truncate (* (exact->inexact
                                             (/ (rt-ref-received filenumber)
                                                len)) 100)))))))

; cannot be threaded, group adding will fail if threaded
(define on-group-invite
  (λ (mtox friendnumber type data len userdata)
    (let* ([friendname (get-contact-name friendnumber)]
           [mbox (message-box "Blight - Groupchat Invite"
                              (string-append friendname
                                             " has invited you to a groupchat!")
                              #f
                              (list 'ok-cancel 'caution))])
      (when (eq? mbox 'ok)
        ; cannot have its own thread
        ; audio.cpp, line 257
        (define join-av-cb
          (λ (mtox-cb grpnum peernum pcm samples channels sample-rate userdata)
            (let ([window (contact-data-window (hash-ref cur-groups grpnum))]
                  [alsource
                   (list-ref (contact-data-alsources
                              (hash-ref cur-groups grpnum)) peernum)])
              (unless (send window speakers-muted?)
                (call/cc
                 (λ (break)
                   ; my daft OpenAL test way
                   ; lots of static and clicking, nothing intelligible
                   #|(displayln 'on-av-cb)
                      (define albuf (car (gen-buffers 1)))
                      
                      (buffer-data albuf (if (= channels 1)
                                             AL_FORMAT_MONO16
                                             AL_FORMAT_STEREO16)
                                   pcm sample-rate)
                      ;(set-source-buffer! alsource albuf)
                      (source-queue-buffers! alsource (list albuf))
                      (play-source alsource)
                      (delete-buffers! (list albuf))|#
                   
                   ; the qtox way
                   ; is this making things segfault?
                   (define processed (source-buffers-processed alsource))
                   (define queued (source-buffers-queued alsource))
                   (define albuf #f)
                   
                   (set-source-looping! alsource AL_FALSE)
                   
                   (printf "join-av-cb: processed: ~a, queued: ~a "
                           processed queued)
                   
                   (cond [(not (zero? processed))
                          (define albufs (make-list processed 0))
                          ;(define albufs (gen-sources processed))
                          ;(define albuf-ptr (malloc processed 'atomic))
                          (define unqbufs (source-unqueue-buffers!! alsource processed albufs))
                          ;(source-unqueue-buffers! alsource albufs)
                          (printf "albufs: ~s unqbufs: ~s " albufs unqbufs)
                          (delete-buffers! unqbufs)
                          (set! albuf (car unqbufs))
                          (printf "albuf: ~a " albuf)]
                         [(< queued 16)
                          (set! albuf (car (gen-buffers 1)))
                          (printf "albuf: ~a " albuf)]
                         [else
                          (displayln "Audio: frame dropped.")
                          (break)])
                   
                   (buffer-data albuf
                                (if (= channels 1)
                                    AL_FORMAT_MONO16
                                    AL_FORMAT_STEREO16)
                                data
                                sample-rate)
                   (source-queue-buffers! alsource (list albuf))
                   (define state (source-source-state alsource))
                   (printf "state: ~a~n" state)
                   
                   (unless (= state AL_PLAYING)
                     (play-source alsource))
                   
                   ; the libblight way (outsourced qtox way)
                   ; proven to work, but outsourced C library is soooo duuuuumb
                   ;(play-audio-buffer alsource pcm samples channels sample-rate)
                   
                   (tox-do mtox-cb)
                   (sleep (/ (tox-do-interval mtox-cb) 1000))))))))
        
        (define grp-number
          (cond [(= type (_TOX_GROUPCHAT_TYPE 'TEXT))
                 (join-groupchat mtox friendnumber data len)]
                [(= type (_TOX_GROUPCHAT_TYPE 'AV))
                 (join-av-groupchat mtox friendnumber data len join-av-cb)]))
        
        (cond [(false? grp-number)
               (message-box "Blight - Groupchat Failure"
                            "Failed to add groupchat!"
                            #f
                            (list 'ok 'stop))]
              [else
               (printf "adding GC: ~a\n" grp-number)
               (flush-output)
               (do-add-group (format "Groupchat #~a" (hash-count cur-groups))
                             grp-number (_TOX_GROUPCHAT_TYPE 'AV))])))))

(define on-group-message
  (λ (mtox groupnumber peernumber message len userdata)
    (let* ([window (contact-data-window (hash-ref cur-groups groupnumber))]
           [name-bytes (get-group-peername mtox groupnumber peernumber)]
           [name (bytes->string/utf-8 name-bytes)]
           [msg-history (send window get-msg-history)])
      (send msg-history add-recv-message (my-name) message name (get-time)))))

(define on-group-action
  (λ (mtox groupnumber peernumber action len userdata)
    (let* ([window (contact-data-window (hash-ref cur-groups groupnumber))]
           [name-bytes (get-group-peername mtox groupnumber peernumber)]
           [msg-history (send window get-msg-history)]
           [name (bytes->string/utf-8 name-bytes)])
      
      (send msg-history add-recv-action action name (get-time)))))

(define on-group-title-change
  (λ (mtox groupnumber peernumber title len userdata)
    (let* ([window (contact-data-window (hash-ref cur-groups groupnumber))]
           [editor (send window get-receive-editor)]
           [gsnip (get-group-snip groupnumber)]
           [newtitle (bytes->string/utf-8 (subbytes title 0 len))])
      (unless (= -1 peernumber)
        (define name-bytes (get-group-peername mtox groupnumber peernumber))
        (define name (bytes->string/utf-8 name-bytes))
        (send editor insert (format "** [~a]: ~a has set the title to `~a'~n"
                                    (get-time) name newtitle)))
      (send gsnip set-status-msg newtitle)
      (send window set-new-label
            (format "Blight - Groupchat #~a: ~a" groupnumber newtitle)))))

(define on-group-namelist-change
  (λ (mtox groupnumber peernumber change userdata)
     (let* ([grp (hash-ref cur-groups groupnumber)]
            [group-window (contact-data-window grp)]
            [lbox (send group-window get-list-box)]
            [sources (contact-data-alsources grp)])
       (cond [(= change (_TOX_CHAT_CHANGE_PEER 'ADD))
              (define name-bytes (get-group-peername mtox groupnumber peernumber))
              (define name (bytes->string/utf-8 name-bytes))
              (send lbox append name)
              (send lbox set-label
                    (format "~a Peers" (get-group-number-peers mtox groupnumber)))
              ; add an al source
              (set-contact-data-alsources! grp (append sources (gen-sources 1)))]
             [(= change (_TOX_CHAT_CHANGE_PEER 'DEL))
              (send lbox delete peernumber)
              (send lbox set-label
                    (format "~a Peers" (get-group-number-peers mtox groupnumber)))
              ; delete an al source
              (let-values ([(h t) (split-at sources peernumber)])
                (delete-sources! (list (car t)))
                (set-contact-data-alsources! grp (append h (cdr t))))]
             [(= change (_TOX_CHAT_CHANGE_PEER 'NAME))
              (define name-bytes (get-group-peername mtox groupnumber peernumber))
              (define name (bytes->string/utf-8 name-bytes))
              (send lbox set-string peernumber name)]))))

(define on-avatar-info
  (λ (mtox friendnumber img-format img-hash userdata)
    ; if the img-format is 'NONE or the image hash isn't the right size,
    ; ignore the whole thing and do nothing
    (unless (or (= (_TOX_AVATAR_FORMAT 'NONE) img-format)
                (< (bytes-length img-hash) TOX_HASH_LENGTH))
      (let* ([window (contact-data-window (hash-ref cur-buddies friendnumber))]
             [friend-id (send window get-key)]
             [hash-file (build-path
                         avatar-dir
                         (string-append friend-id ".hash"))]
             [png-file (build-path
                        avatar-dir
                        (string-append friend-id ".png"))]
             [cropped-hash (subbytes img-hash 0 TOX_HASH_LENGTH)])
        ; check if we have the avatar already
        (cond [(and (file-exists? hash-file)
                    (file-exists? png-file))
               ; if they both exist, do nothing if the hashes are identical
               (unless (bytes=? (file->bytes hash-file #:mode 'binary) cropped-hash)
                 (displayln "The avatar's hash hash changed! Updating...")
                 ; request the avatar's data
                 (request-avatar-data mtox friendnumber)
                 ; update the hash file
                 (let ([hash-port-out (open-output-file hash-file
                                                        #:mode 'binary
                                                        #:exists 'truncate/replace)])
                   (write-bytes cropped-hash hash-port-out)
                   (close-output-port hash-port-out)))]
              [else
               (displayln "We got a new avatar! Saving information...")
               ; request the avatar's data
               (request-avatar-data mtox friendnumber)
               ; update the hash file
               (let ([hash-port-out (open-output-file hash-file
                                                      #:mode 'binary
                                                      #:exists 'truncate/replace)])
                 (write-bytes cropped-hash hash-port-out)
                 (close-output-port hash-port-out))])))))

(define on-avatar-data
  (λ (mtox friendnumber img-format img-hash data-ptr datalen userdata)
    (unless (= img-format (_TOX_AVATAR_FORMAT 'NONE))
      (let* ([window (contact-data-window (hash-ref cur-buddies friendnumber))]
             [friend-id (send window get-key)]
             [png-file (build-path
                        avatar-dir
                        (string-append friend-id ".png"))]
             [png-port-out (open-output-file png-file
                                             #:mode 'binary
                                             #:exists 'truncate/replace)]
             [data-bytes (make-sized-byte-string data-ptr datalen)])
        ; write to file
        (write-bytes data-bytes png-port-out 0 datalen)
        ; close the output port
        (close-output-port png-port-out)
        ; tell the buddy window to update the avatar
        (send window set-friend-avatar png-file)))))

(define on-typing-change
  (λ (mtox friendnumber typing? userdata)
    (let ([window (contact-data-window (hash-ref cur-buddies friendnumber))])
      (send window is-typing? typing?))))

; we are receiving a call, phone is ringing
(define on-audio-invite
  (λ (mav call-index arg)
    (displayln 'on-audio-invite)
    (printf "agent: ~a call-index: ~a arg: ~a~n"
            mav call-index arg)
    (when (make-noise)
      (play-sound (ninth sounds) #t))
    (av-answer my-av call-index my-csettings)))

; we are calling someone, phone is ringing
(define on-audio-ringing
  (λ (mav call-index arg)
    (displayln 'on-audio-ringing)
    (printf "agent: ~a call-index: ~a arg: ~a~n"
            mav call-index arg)
    (when (make-noise)
      (play-sound (tenth sounds) #t))))

; helper procedure to prepare our call
; type is ignored at the moment
(define prepare-call
  (λ (mav call-index friend-id csettings type)
    (debug-prefix "Audio: ")
    (dprintf "Preparing call ~a~n" call-index)
    (do-add-call call-index friend-id csettings type)))

; call has connected, rtp transmission has started
(define on-audio-start
  (λ (mav call-index arg)
    (let ([friend-id (get-peer-id mav call-index 0)])
      (unless (< friend-id 0)
        (define peer-csettings (get-peer-csettings mav call-index friend-id))
        (cond [(negative? (first peer-csettings))
               (debug-prefix "Audio: ")
               (dprintf "Problem starting audio; error code ~a~n" peer-csettings)]
              [else
               (prepare-call mav call-index friend-id
                             (second peer-csettings) (first peer-csettings))])))))

; the side that initiated the call has canceled the invite
(define on-audio-cancel
  (λ (mav call-index arg)
    (displayln 'on-audio-cancel)
    (debug-prefix "Audio: ")
    (dprintf "Call ~a cancelled.~n" call-index)))

; the side that was invited rejected the call
(define on-audio-reject
  (λ (mav call-index arg)
    (displayln 'on-audio-reject)
    (debug-prefix "Audio: ")
    (dprintf "Call ~a rejected.~n" call-index)))

; the call that was active has ended
(define on-audio-end
  (λ (mav call-index arg)
    (displayln 'on-audio-end)
    (debug-prefix "Audio: ")
    (dprintf "Deleting call ~a.~n" call-index)
    (do-delete-call call-index)))

; when the request didn't get a response in time
(define on-audio-request-timeout
  (λ (mav call-index arg)
    (displayln 'on-audio-request-timeout)))

; peer timed out, stop the call
(define on-audio-peer-timeout
  (λ (mav call-index arg)
    (displayln 'on-audio-peer-timeout)
    (debug-prefix "Audio: ")
    (dprintf "Peer timeout, deleting call ~a.~n" call-index)
    (do-delete-call call-index)))

; peer changed csettings. prepare for changed av
(define on-audio-peer-cschange
  (λ (mav call-index arg)
    (displayln 'on-audio-peer-cschange)))

; csettings change confirmation. once triggered, peer will be ready
; to receive changed av
(define on-audio-self-cschange
  (λ (mav call-index arg)
    (displayln 'on-audio-self-cschange)))

; we are receiving audio
(define on-audio-receive
  (λ (mav call-index pcm size data)
    (displayln 'on-audio-receive)
    (printf "on-audio-receive: agent: ~a call-index: ~a pcm: ~a size: ~a data: ~a~n"
            mav call-index pcm size data)))
#| ################# END CALLBACK PROCEDURE DEFINITIONS ################# |#

; register our callback functions
(callback-friend-request my-tox on-friend-request)
(callback-friend-message my-tox on-friend-message)
(callback-friend-action my-tox on-friend-action)
(callback-name-change my-tox on-friend-name-change)
(callback-user-status my-tox on-status-type-change)
(callback-connection-status my-tox on-connection-status-change)
(callback-file-send-request my-tox on-file-send-request)
(callback-file-control my-tox on-file-control)
(callback-file-data my-tox on-file-data)
(callback-group-invite my-tox on-group-invite)
(callback-group-message my-tox on-group-message)
(callback-group-action my-tox on-group-action)
(callback-group-title my-tox on-group-title-change)
(callback-group-namelist-change my-tox on-group-namelist-change)
(callback-avatar-info my-tox on-avatar-info)
(callback-avatar-data my-tox on-avatar-data)
(callback-typing-change my-tox on-typing-change)
(callback-callstate my-av on-audio-invite (_ToxAvCallbackID 'Invite))
(callback-callstate my-av on-audio-ringing (_ToxAvCallbackID 'Ringing))
(callback-callstate my-av on-audio-start (_ToxAvCallbackID 'Start))
(callback-callstate my-av on-audio-cancel (_ToxAvCallbackID 'Cancel))
(callback-callstate my-av on-audio-reject (_ToxAvCallbackID 'Reject))
(callback-callstate my-av on-audio-end (_ToxAvCallbackID 'End))
(callback-callstate my-av on-audio-request-timeout (_ToxAvCallbackID 'RequestTimeout))
(callback-callstate my-av on-audio-peer-timeout (_ToxAvCallbackID 'PeerTimeout))
(callback-callstate my-av on-audio-peer-cschange (_ToxAvCallbackID 'PeerCSChange))
(callback-callstate my-av on-audio-self-cschange (_ToxAvCallbackID 'SelfCSChange))
(callback-audio-recv my-av on-audio-receive)
