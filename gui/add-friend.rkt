#lang racket/gui
; add-friend.rkt
(require libtoxcore-racket/functions
         libtoxcore-racket/enums
         "frame.rkt"
         "friend-list.rkt"
         "smart-list.rkt"
         "../blight.rkt"
         "../config.rkt"
         "../helpers.rkt"
         "../tox.rkt"
         "../utils.rkt"
         "../dns/toxdns.rkt")

(provide (all-defined-out))

#| #################### BEGIN FRIEND STUFF ####################### |#
(define add-friend-box (new dialog%
                            [label "Blight - Add a new Tox friend"]
                            [style (list 'close-button)]))

(define dns-msg (new message%
                     [parent add-friend-box]
                     [label "DNS nickname:"]))

(define dns-panel (new horizontal-panel%
                       [parent add-friend-box]
                       [alignment '(center center)]))

(define add-friend-txt-tfield (new text-field%
                                   [parent dns-panel]
                                   [label #f]
                                   [min-width 38]))

; choices for status type changes
(define dns-domain-choice
  (new choice%
       [parent dns-panel]
       [label #f]
       [choices '("toxme.se"
                  "utox.org")]))

(define hex-message (new message%
                         [parent add-friend-box]
                         [label "Friend ID(X):"]))

(define hex-panel (new horizontal-panel%
                       [parent add-friend-box]
                       [alignment '(center center)]))

; add friend with Tox ID
(define add-friend-hex-tfield (new text-field%
                                   [parent hex-panel]
                                   [label #f]
                                   [min-width 38]
                                   [callback (λ (l e)
                                               (if (tox-id? (send l get-value))
                                                   (send hex-message set-label
                                                         "Friend ID(✓):")
                                                   (send hex-message set-label
                                                         "Friend ID(X):")))]))

(define message-message (new message%
                             [parent add-friend-box]
                             [label "Message:"]))

(define message-panel (new horizontal-panel%
                           [parent add-friend-box]
                           [alignment '(center center)]))

; message to send as a friend request
(define add-friend-message-tfield
  (new text-field%
       [parent message-panel]
       [label #f]
       [min-width 38]
       [init-value "Please let me add you to my contact list"]))

; panel for the buttons
(define add-friend-panel (new horizontal-panel%
                              [parent add-friend-box]
                              [alignment '(right center)]))

(define add-friend-error-dialog (new dialog%
                                     [label "Invalid Tox ID"]
                                     [style (list 'close-button)]))

; don't actually want to add a friend right now
(define add-friend-cancel-button
  (new button%
       [parent add-friend-panel]
       [label "Cancel"]
       [callback (λ (button event)
                   (send add-friend-hex-tfield set-value "")
                   (send add-friend-txt-tfield set-value "")
                   (send add-friend-box show #f))]))

; OK button for add-friend dialog box
(define add-friend-ok-button
  (new button%
       [parent add-friend-panel]
       [label "OK"]
       [callback (λ (button event)
                   (let* ([nick-tfield (send add-friend-txt-tfield get-value)]
                          [hex-tfield (send add-friend-hex-tfield get-value)]
                          [message-str (send add-friend-message-tfield get-value)]
                          [message-bytes (string->bytes/utf-8 message-str)]
                          [domain (send dns-domain-choice get-string-selection)])
                     ; add the friend to the friend list
                     (cond [(or
                             ; the hex field is empty, nick field cannot be empty
                             (and (string=? hex-tfield "")
                                  (and (not (string=? nick-tfield ""))
                                       ; make sure we get a response from the DNS
                                       (not (false? (tox-dns1 nick-tfield domain)))))
                             ; the nick field is empty, hex field cannot be empty
                             (and (string=? nick-tfield "")
                                  ; make sure hex field is a proper tox id
                                  (tox-id? hex-tfield)))
                            ; convert hex to bytes
                            (define nick-bytes (make-bytes TOX_ADDRESS_SIZE))
                            ; we're doing a direct friend add
                            (cond [(string=? nick-tfield "")
                                   ; obtain the byte form of the id
                                   (set! nick-bytes
                                         (hex-string->bytes
                                          hex-tfield
                                          TOX_ADDRESS_SIZE))]
                                  ; we're doing a dns lookup
                                  [(string=? hex-tfield "")
                                   ; obtain the id from the dns query
                                   (define friend-hex (tox-dns1 nick-tfield domain))
                                   ; obtain the byte form of the id
                                   (set! nick-bytes
                                         (hex-string->bytes
                                          friend-hex
                                          TOX_ADDRESS_SIZE))])
                            ; check message for its length
                            (when (> (bytes-length message-bytes) TOX_MAX_FRIEND_REQUEST_LENGTH)
                              (set! message-bytes
                                    (subbytes message-bytes
                                              0
                                              TOX_MAX_FRIEND_REQUEST_LENGTH)))
                            (let* ([result (friend-add my-tox
                                                       nick-bytes
                                                       message-bytes)]
                                   [num (first result)]
                                   [err (bytes-ref (second result) 0)])
                              ; check for all the friend add errors
                              (cond [(= err (_TOX_ERR_FRIEND_ADD 'NULL))
                                     (displayln "ERROR: TOX_ERR_FRIEND_ADD_NULL")
                                     [(= err (_TOX_ERR_FRIEND_ADD 'TOO_LONG))
                                      (displayln "ERROR: TOX_ERR_FRIEND_ADD_TOOLONG")
                                      (when (make-noise)
                                        (play-sound (last sounds) #t))]
                                     [(= err (_TOX_ERR_FRIEND_ADD 'NO_MESSAGE))
                                      (displayln "ERROR: TOX_ERR_FRIEND_ADD_NO_MESSAGE")
                                      (when (make-noise)
                                        (play-sound (last sounds) #t))]
                                     [(= err (_TOX_ERR_FRIEND_ADD 'OWN_KEY))
                                      (displayln "ERROR: TOX_ERR_FRIEND_ADD_OWN_KEY")
                                      (when (make-noise)
                                        (play-sound (last sounds) #t))]
                                     [(= err (_TOX_ERR_FRIEND_ADD 'ALREADY_SENT))
                                      (displayln "ERROR: TOX_ERR_FRIEND_ADD_ALREADY_SENT")
                                      (when (make-noise)
                                        (play-sound (last sounds) #t))]
                                     (when (make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_ERR_FRIEND_ADD 'BAD_CHECKSUM))
                                     (displayln "ERROR: TOX_ERR_FRIEND_ADD_BAD_CHECKSUM")
                                     (when (make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_ERR_FRIEND_ADD 'SET_NEW_NOSPAM))
                                     (displayln "ERROR: TOX_ERR_FRIEND_ADD_SET_NEW_NOSPAM")
                                     (when (make-noise)
                                       (play-sound (last sounds) #t))]
                                    [(= err (_TOX_ERR_FRIEND_ADD 'MALLOC))
                                     (displayln "ERROR: TOX_ERR_FRIEND_ADD_MALLOC")
                                     (when (make-noise)
                                       (play-sound (last sounds) #t))]
                                    [else (displayln "All okay!")
                                          ; save the tox data
                                          (blight-save-data)
                                          
                                          (let* ([newfn num];(sub1 (self-friend-list-size my-tox))]
                                                 [key (friend-key my-tox newfn)])
                                            (if (string=? hex-tfield "")
                                                (create-buddy nick-tfield key)
                                                (create-buddy
                                                 (format "Anonymous (~a)"
                                                         (substring hex-tfield 0 5)) key)))
                                          
                                          ; update friend list, but don't mess up
                                          ; the numbering we already have
                                          
                                          ; zero-out some fields
                                          (send add-friend-hex-tfield set-value "")
                                          (send add-friend-txt-tfield set-value "")
                                          ; close the window
                                          (send add-friend-box show #f)
                                          ; the invite list needs to be updated for
                                          ; the groupchat windows that still exist
                                          (unless (zero? (hash-count cur-groups))
                                            (update-invite-list))]))]
                           ; something went wrong!
                           [else (when (make-noise)
                                   (play-sound (last sounds) #t))
                                 (let ([mbox (message-box
                                              "Blight - Invalid Tox ID"
                                              "Sorry, that is an invalid Tox ID or DNS nick."
                                              add-friend-error-dialog
                                              (list 'ok 'stop))])
                                   (when (eq? mbox 'ok)
                                     (send add-friend-error-dialog show #f)))])))]))

; send friend request
(define add-friend-button (new button%
                               [parent panel]
                               [label "Add friend"]
                               [callback (λ (button event)
                                           (send add-friend-box show #t))]))

; remove friend from list
(define delete-friend-button
  (new button%
       [parent panel]
       [label "Del friend"]
       [callback (λ (button event)
                    (let ([friend-num (contact-data-tox-num (send sml get-selection-cd))])
                     (delete-friend friend-num)))]))
#| ##################### END FRIEND STUFF ####################### |#
