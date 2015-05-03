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

(provide on-friend-status)

#| ################# START CALLBACK PROCEDURE DEFINITIONS ################# |#

; TODO:
; self-connection-status indicator of our connection status

; set all the callback functions
(define on-self-connection-status
  (λ (mtox connection-status userdata)
    (cond [(eq? connection-status 'none)
           (displayln "We're not connected to the network right now.")]
          [(eq? connection-status 'tcp)
           (displayln "We're connected to the network via TCP.")]
          [(eq? connection-status 'udp)
           (displayln "We're connected to the network via UDP.")])))

(define on-friend-request
  (λ (mtox public-key message message-len userdata)
    (unless (< (bytes-length public-key) TOX_ADDRESS_SIZE)
      ; make sure public-key is the correct size...
      (define pubkey (subbytes public-key 0 TOX_ADDRESS_SIZE))
      ; convert pubkey from bytes to string so we can display it
      (define id-hex (bytes->hex-string pubkey))
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
                (display "Adding friend... ")
                (define-values (friendnumber err) (friend-add-norequest mtox pubkey))
                
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
                  (for ([i (self-friend-list-size mtox)])
                    (let-values ([(conn-status err) (friend-connection-status mtox i)])
                      (status-checker i conn-status)))
                  ; the invite list needs to be updated for
                  ; the groupchat windows that still exist
                  (unless (zero? (hash-count cur-groups))
                    (update-invite-list))
                  ; save the tox data
                  (blight-save-data))
                
                ; catch errors
                (cond [(eq? err 'ok) (add-friend-success)]
                      [else
                       (display "There was an error accepting the friend request! ")
                       ; if we've failed, try again 3(?) more times
                       (let loop ([tries 0])
                         (cond [(= tries 3)
                                (displayln "Failed!")
                                (when (make-noise)
                                  (play-sound (last sounds) #t))]
                               [else
                                (display "Retrying... ")
                                (iterate mtox)
                                (sleep (/ (iteration-interval mtox) 1000))
                                (let-values ([(num err) (friend-add-norequest mtox pubkey)])
                                  (if (eq? err 'ok)
                                      (begin
                                        (displayln "Success!")
                                        (add-friend-success))
                                      (loop (add1 tries))))]))])
                (send fr-dialog show #f))]))
      
      (send fr-text insert (string-append
                            id-hex
                            "\nwould like to add you as a friend!\n"
                            "Message: " message))
      (send fr-dialog show #t))))

; message is a string
(define on-friend-message
  (λ (mtox friendnumber type message len userdata)
    (unless (zero? (string-length message))
      (let* ([window (get-contact-window friendnumber)]
             [msg-history (send window get-msg-history)]
             [name (send window get-name)])
        ; if the window isn't open, force it open
        (cond [(not (send window is-shown?)) (send window show #t)])
        
        (if (eq? type 'normal)
            (send msg-history add-recv-message (my-name) message name (get-time))
            (send msg-history add-recv-action message name (get-time)))
        
        ; make a noise
        (when (make-noise)
          (play-sound (first sounds) #t))
        
        ; add a little asterisk in the frame title to show we've gotten new messages
        (when (false? (send window window-has-focus?)) (send window set-msg-unread))
        
        ; add message to the history database
        (if (eq? type 'normal)
            (add-history (my-id-hex) (send window get-key) message 0)
            (add-history (my-id-hex) (send window get-key)
                         (string-append "ACTION: " message) 0))))))

(define on-friend-name
  (λ (mtox friendnumber newname newname-len userdata)
    (let ([sn (get-contact-snip friendnumber)])
      (send sml rename-entry sn newname))
    
    (let-values ([(window) (get-contact-window friendnumber)]
                 [(conn-status conn-err) (friend-connection-status mtox friendnumber)])
      ; update the name in the list
      (send window set-name newname)
      ; update the name in the window
      (send window set-new-label (string-append "Blight - " newname))
      ; add connection status icon
      (status-checker friendnumber conn-status))))

(define on-friend-status-message
  (λ (mtox friendnumber status-message message-len userdata)
    ; from friend-list
    (update-contact-status-msg friendnumber status-message)))

(define on-friend-status
  (λ (mtox friendnumber status userdata)
    (send (get-contact-snip friendnumber) set-status status)
    (update-contact-status friendnumber status)
    ; friend is online
    #;(cond [(eq? status 'available)
           (send (get-contact-snip friendnumber) set-status 'available)
           (update-contact-status friendnumber 'available)]
          ; friend is away
          [(= status (_TOX_USER_STATUS 'AWAY))
           (send (get-contact-snip friendnumber) set-status 'away)
           (update-contact-status friendnumber 'away)]
          ; friend is busy
          [(= status (_TOX_USER_STATUS 'BUSY))
           (send (get-contact-snip friendnumber) set-status 'busy)
           (update-contact-status friendnumber 'busy)])))

(define on-friend-connection-status-change
  (λ (mtox friendnumber status userdata)
    ; add a thingie that shows the friend is online
    (cond [(eq? status 'none)
           (send (get-contact-snip friendnumber) set-status 'offline)
           (update-contact-status friendnumber 'offline)
           (when (make-noise)
             (play-sound (third sounds) #t))]
          [else
           (send (get-contact-snip friendnumber) set-status 'available)
           (update-contact-status friendnumber 'available)
           (when (make-noise)
             (play-sound (second sounds) #t))
           ; send our newly-online friend our avatar information, if applicable
           (let* ([my-pubkey (substring (my-id-hex) 0 (* TOX_PUBLIC_KEY_SIZE 2))]
                  [avatar-name (string-append my-pubkey ".png")]
                  [avatar-file (build-path avatar-dir avatar-name)])
             (when (file-exists? avatar-file)
               (define-values (hash-success file-hash)
                 (tox-hash (file->bytes avatar-file #:mode 'binary)))
               ; file hash is its ID
               (define-values (filenum file-err)
                 (file-send mtox friendnumber
                            'avatar
                            (file-size avatar-file)
                            file-hash
                            (string->bytes/utf-8 avatar-name)))
               (transfers-add! mtox friendnumber filenum file-hash avatar-file
                               (file->bytes avatar-file))))])))

; a control action has been applied to a file transfer
(define on-file-recv-control
  (λ (mtox friendnumber filenumber control-type userdata)
    (let* ([window (get-contact-window friendnumber)]
           [receive-editor (send window get-receive-editor)]
           [fc-lb (send window get-fc-lb)]
           [msg-history (send window get-msg-history)]
           [update-fc-lb (λ ()
                           (send fc-lb set
                                 (sort (map (λ (x)
                                              (string-append
                                               (number->string (transfers-ref-num (car x)))
                                               ": "
                                               (transfers-ref-filename (car x))))
                                            (hash->list transfers))
                                       string<?)))])
      (with-handlers
          ([exn:blight:transfer?
            (lambda (ex)
              (blight-handle-exception ex)
              (send msg-history send-file-recv-error (exn-message ex)))])
        ; cue that we're going to be sending the data now
        (cond [(eq? control-type 'resume)
               ; update file control list box
               (update-fc-lb)]
              ; the transfer has been canceled, close everything up
              [(eq? control-type 'cancel)
               (define-values (id-success id-err f-id)
                 (file-id mtox friendnumber filenumber))
               ; remove transfer from hash
               (transfers-del! f-id)
               (update-fc-lb)]
              ; catch everything else and just update both of the list boxes
              [else (update-fc-lb)])))))

; our friend is requesting we send them a chunk of data
(define on-file-chunk-request
  (λ (mtox friendnumber filenumber pos chunk-len userdata)
    (let* ([window (get-contact-window friendnumber)]
           [fc-lb (send window get-fc-lb)]
           [update-fc-lb (λ ()
                           (send fc-lb set
                                 (sort (map (λ (x)
                                              (string-append
                                               (number->string (transfers-ref-num (car x)))
                                               ": "
                                               (transfers-ref-filename (car x))))
                                            (hash->list transfers))
                                       string<?)))])
      
      (define-values (id-success id-err f-id) (file-id mtox friendnumber filenumber))
      (cond
        ; the transfer is complete, close transfer stuff
        [(zero? chunk-len) (transfers-del! f-id) (update-fc-lb)]
        ; otherwise, send the chunk and update our position
        [else (let ([chunk (subbytes (transfers-ref-data f-id) pos (+ pos chunk-len))])
                (file-send-chunk mtox friendnumber filenumber pos chunk chunk-len))
              (set-transfers-pos! f-id pos)]))))

; our friend wants to send us data
; needs to be in its own thread, otherwise we'll d/c(?)
; perhaps, instead of identifying file transfers by filenumber,
; they are identified by file-id
; (define fid (file-id mtox friendnumber filenumber))
(define on-file-recv-request
  (λ (mtox friendnumber filenumber kind filesize filename fname-len userdata)
    (thread
     (λ ()
       (if (eq? kind 'data)
           ; regular data
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
             (when (make-noise)
               (play-sound (seventh sounds) #t))
             (cond
               [(eq? mbox 'ok)
                (let ([path (put-file "Select a file"
                                      #f
                                      download-path
                                      filename)]
                      [window (get-contact-window friendnumber)])
                  (unless (false? path)
                    (define receive-editor
                      (send window get-receive-editor))
                    (file-control mtox friendnumber filenumber 'resume)
                    (define-values (id-success id-err f-id)
                      (file-id mtox friendnumber filenumber))
                    (send window set-gauge-pos 0)
                    (transfers-add! mtox friendnumber filenumber f-id path #"" 0
                                    (open-output-file path
                                                      #:mode 'binary
                                                      #:exists 'replace))
                    (send msg-history
                          begin-recv-file path (get-time))))]
               [else (file-control mtox friendnumber filenumber 'cancel)]))
           
           ; auto-accept avatar data
           ; the name of the avatar is friend-public-key.ext
           ;
           ; if the filesize is greater than the client max or if the file is not a PNG
           ; image, do not accept it, obviously.
           (unless (or (> filesize BLIGHT-MAX-AVATAR-SIZE)
                       (false? filename)
                       (not (bytes=? #"png" (filename-extension filename))))
             (let* ([window (contact-data-window (hash-ref cur-buddies friendnumber))]
                    [friend-id (send window get-key)]
                    [hash-path (build-path avatar-dir (string-append friend-id ".hash"))]
                    [avatar-path (build-path avatar-dir (string-append friend-id ".png"))])
               ; grab the file-id (AKA file hash)
               (define-values (success err f-id) (file-id mtox friendnumber filenumber))
               (cond [(zero? filesize)
                      ; the avatar is not set, delete any avatar we have cached
                      (when (file-exists? avatar-path)
                        (delete-file avatar-path)
                        (send window set-friend-avatar #f))]
                     [(and (file-exists? hash-path) (file-exists? avatar-path))
                      ; if both files exist and their hashes are identical, do nothing
                      (unless (bytes=? (file->bytes hash-path #:mode 'binary) f-id)
                        (displayln "The avatar's hash has changed! Updating...")
                        ; start the file transfer
                        (file-control mtox friendnumber filenumber 'resume)
                        (transfers-add! mtox friendnumber filenumber f-id avatar-path #"" 0
                                        (open-output-file avatar-path
                                                          #:mode 'binary
                                                          #:exists 'replace)))]
                     [else
                      ; we have only one of avatar or hash file or neither
                      (displayln "We got a new avatar! Saving information...")
                      (file-control mtox friendnumber filenumber 'resume)
                      (transfers-add! mtox friendnumber filenumber f-id avatar-path #"" 0
                                      (open-output-file avatar-path
                                                        #:mode 'binary
                                                        #:exists 'replace))]))))))))

; our friend has sent us a chunk of data
(define on-file-recv-chunk
  (λ (mtox friendnumber filenumber position chunk chunk-len userdata)
    (let* ([window (get-contact-window friendnumber)]
           [fc-lb (send window get-fc-lb)]
           [update-fc-lb (λ ()
                           (send fc-lb set
                                 (sort (map (λ (x)
                                              (string-append
                                               (number->string (transfers-ref-num (car x)))
                                               ": "
                                               (transfers-ref-filename (car x))))
                                            (hash->list transfers))
                                       string<?)))]
           [msg-history (send window get-msg-history)])
      (define-values (id-success id-err f-id)
        (file-id mtox friendnumber filenumber))
      
      (with-handlers
          ([exn:blight:transfer?
            (lambda (ex)
              (send msg-history send-file-recv-error (exn-message ex)))])
        (cond
          ; file transfer is complete, close up transfer
          [(zero? chunk-len) (transfers-del! f-id) (update-fc-lb)]
          [else
           (write-bytes chunk (transfers-ref-fhandle f-id))
           (set-transfers-pos! f-id position)
           (send window set-gauge-pos
                 (fl->exact-integer (truncate (* (exact->inexact
                                                  (/ (transfers-ref-pos f-id)
                                                     chunk-len)) 100))))])))))

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
                   
                   (iterate mtox-cb)
                   (sleep (/ (iteration-interval mtox-cb) 1000))))))))
        
        (define grp-number
          (cond [(eq? type 'text)
                 (join-groupchat mtox friendnumber data len)]
                [(eq? type 'av)
                 (join-av-groupchat mtox friendnumber data len join-av-cb)]))
        
        (printf "on-group-invite; type: ~s, grp-number: ~s~n" type grp-number)
        
        (cond [(false? grp-number)
               (message-box "Blight - Groupchat Failure"
                            "Failed to add groupchat!"
                            #f
                            (list 'ok 'stop))]
              [else
               (printf "adding GC: ~a\n" grp-number)
               (flush-output)
               (do-add-group (format "Groupchat #~a" (hash-count cur-groups))
                             grp-number 'av)])))))

(define on-group-message
  (λ (mtox groupnumber peernumber message len userdata)
    (let* ([window (contact-data-window (hash-ref cur-groups groupnumber))]
           [name-bytes (group-peername mtox groupnumber peernumber)]
           [name (bytes->string/utf-8 name-bytes)]
           [msg-history (send window get-msg-history)])
      (send msg-history add-recv-message (my-name) message name (get-time)))))

(define on-group-action
  (λ (mtox groupnumber peernumber action len userdata)
    (let* ([window (contact-data-window (hash-ref cur-groups groupnumber))]
           [name-bytes (group-peername mtox groupnumber peernumber)]
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
        (define name-bytes (group-peername mtox groupnumber peernumber))
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
      (cond [(eq? change 'add)
             (define name-bytes (group-peername mtox groupnumber peernumber))
             (define name (bytes->string/utf-8 name-bytes))
             (send lbox append name)
             (send lbox set-label
                   (format "~a Peers" (group-number-peers mtox groupnumber)))
             ; add an al source
             (unless (false? sources)
               (set-contact-data-alsources! grp (append sources (gen-sources 1))))]
            [(eq? change 'del)
             (send lbox delete peernumber)
             (send lbox set-label
                   (format "~a Peers" (group-number-peers mtox groupnumber)))
             ; delete an al source
             (unless (false? sources)
               (let-values ([(h t) (split-at sources peernumber)])
                 (delete-sources! (list (car t)))
                 (set-contact-data-alsources! grp (append h (cdr t)))))]
            [(eq? change 'name)
             (define name-bytes (group-peername mtox groupnumber peernumber))
             (define name (bytes->string/utf-8 name-bytes))
             (send lbox set-string peernumber name)]))))

(define on-friend-typing
  (λ (mtox friendnumber typing? userdata)
    (let ([window (contact-data-window (hash-ref cur-buddies friendnumber))])
      (send window is-typing? typing?))))

(define on-friend-read-receipt
  (λ (mtox friendnumber message-id userdata)
    (let ([window (contact-data-window (hash-ref cur-buddies friendnumber))])
      (printf "on-friend-read-receipt: friend ~a received message ~a\n"
              friendnumber message-id))))

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
(callback-self-connection-status my-tox on-self-connection-status)
(callback-friend-request my-tox on-friend-request)
(callback-friend-message my-tox on-friend-message)
(callback-friend-read-receipt my-tox on-friend-read-receipt)
(callback-friend-name my-tox on-friend-name)
(callback-friend-status-message my-tox on-friend-status-message)
(callback-friend-status my-tox on-friend-status)
(callback-friend-connection-status my-tox on-friend-connection-status-change)
(callback-file-recv-control my-tox on-file-recv-control)
(callback-file-chunk-request my-tox on-file-chunk-request)
(callback-file-recv my-tox on-file-recv-request)
(callback-file-recv-chunk my-tox on-file-recv-chunk)
(callback-group-invite my-tox on-group-invite)
(callback-group-message my-tox on-group-message)
(callback-group-action my-tox on-group-action)
(callback-group-title my-tox on-group-title-change)
(callback-group-namelist-change my-tox on-group-namelist-change)
(callback-friend-typing my-tox on-friend-typing)
(callback-callstate my-av on-audio-invite 'invite)
(callback-callstate my-av on-audio-ringing 'ringing)
(callback-callstate my-av on-audio-start 'start)
(callback-callstate my-av on-audio-cancel 'cancel)
(callback-callstate my-av on-audio-reject 'reject)
(callback-callstate my-av on-audio-end 'end)
(callback-callstate my-av on-audio-request-timeout 'request-timeout)
(callback-callstate my-av on-audio-peer-timeout 'peer-timeout)
(callback-callstate my-av on-audio-peer-cschange 'peer-cs-change)
(callback-callstate my-av on-audio-self-cschange 'self-cs-change)
(callback-audio-recv my-av on-audio-receive)
