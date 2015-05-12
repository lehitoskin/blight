#lang racket/gui
; friend-list.rkt
; contains definitions for the friend list
(require libtoxcore-racket
         mrlib/aligned-pasteboard
         "frame.rkt"
         "chat.rkt"
         "group.rkt"
         "smart-list.rkt"
         "../audio.rkt"
         "../blight.rkt"
         "../config.rkt"
         "../tox.rkt"
         "../utils.rkt")

(provide (except-out (all-defined-out)
                     on-status-type-change))

#| ################## BEGIN FRIEND LIST STUFF #################### |#
(define cs-style (new cs-style-manager))

(define sml
  (new smart-list%))

(define sml-canvas
  (new aligned-editor-canvas%
       [parent frame]
       [editor sml]
       [style (list 'no-hscroll)]
       ; perfect minimum height
       ; needs to be set because of frame-vpanel and frame-hpanel
       [min-height 450]))

(define sml-km (init-smart-list-keymap))
(init-default-smartlist-keymap sml-km)
(send sml set-keymap sml-km)

(send sml set-delete-entry-cb
      (lambda (cd)
        (let ([friend-num (contact-data-tox-num cd)])
          (if (eq? (contact-data-type cd) 'buddy)
              (begin
                (delete-friend friend-num))
              
              (begin
                (do-delete-group! friend-num))))))

(define (get-contact-data friendnumber)
  (hash-ref cur-buddies friendnumber))

(define (get-group-data friendnumber)
  (hash-ref cur-groups friendnumber))

(define (get-contact-snip number)
  (send sml get-entry-by-key
        (contact-data-name (hash-ref cur-buddies number))))

(define (get-group-snip number)
  (send sml get-entry-by-key
        (contact-data-name (hash-ref cur-groups number))))

(define (get-contact-window friendnumber)
  (let* ([cd (get-contact-data friendnumber)])
    (contact-data-window cd)))

(define (get-contact-name friendnumber)
  (let* ([cd (get-contact-data friendnumber)])
    (contact-data-name cd)))

(define (update-contact-status friend-num con-status)
  (define status-msg
    (friend-status-msg-str my-tox friend-num))
  
  (define cd (get-contact-data friend-num))
  (define sn (get-contact-snip friend-num))
  (define window (get-contact-window friend-num))
  
  (send sn set-status con-status)
  (send sn set-status-msg status-msg)
  (send window set-status-msg status-msg))

(define (update-contact-status-msg friend-num status-msg)
  (define cd (get-contact-data friend-num))
  (define sn (get-contact-snip friend-num))
  (define window (get-contact-window friend-num))
  
  (send sn set-status-msg status-msg)
  (send window set-status-msg status-msg))

(define on-status-type-change
  (λ (mtox friendnumber status)
    ; friend is online
    (send (get-contact-snip friendnumber) set-status status)
    (update-contact-status friendnumber status)
    #;(cond [(eq? status 'none)
           (send (get-contact-snip friendnumber) set-status 'available)
           (update-contact-status friendnumber 'available)]
          ; friend is away
          [(= status 'away)
           (send (get-contact-snip friendnumber) set-status 'away)
           (update-contact-status friendnumber 'away)]
          ; friend is busy
          [(= status 'busy)
           (send (get-contact-snip friendnumber) set-status 'busy)
           (update-contact-status friendnumber 'busy)])))

; helper to avoid spamming notification sounds
(define status-checker
  (λ (friendnumber conn-status)
    (let-values ([(status-type status-err) (friend-status my-tox friendnumber)])
      (cond [(eq? conn-status 'none)
             (send (get-contact-snip friendnumber) set-status 'offline)
             (update-contact-status friendnumber 'offline)]
            
            ; user is online, check his status type
            [else (on-status-type-change my-tox friendnumber status-type)]))))

;; helper to get friend name as return value
(define (friend-name-str tox num)
  (define-values (success err name-bytes) (friend-name tox num))
  (bytes->string/utf-8 name-bytes))

; helper to get friend's status message as a return value
(define (friend-status-msg-str tox num)
  (define-values (success err status-msg) (friend-status-message tox num))
  (bytes->string/utf-8 status-msg))

;; helper to get friend key as return value
(define (friend-key tox num)
  (define-values (success err pubkey) (friend-public-key tox num))
  (bytes->hex-string pubkey))

;; helper to get friend number without ->bytes conversion
(define (friend-number tox key)
  (define-values (number err)
    (friend-by-public-key tox (hex-string->bytes key TOX_PUBLIC_KEY_SIZE)))
  number)

(define (update-invite-list)
  (for ([(num grp) cur-groups])
    (let ([cw (contact-data-window grp)])
      (send cw
            update-invite-list))))

(define (create-buddy name key)
  (let* ([avatar-file (build-path avatar-dir
                                  (string-append key ".png"))]
         [avatar-bitmap (if (file-exists? avatar-file)
                            (make-object bitmap% avatar-file)
                            #f)]
         [bitmap-height (if (false? avatar-bitmap)
                            300
                            (send avatar-bitmap get-height))]
         [bitmap-width (if (false? avatar-bitmap)
                           300
                           (send avatar-bitmap get-width))]
         [chat-window (new chat-window%
                           [this-label (format "Blight - ~a" name)]
                           [this-height 400]
                           [this-width 600]
                           [avatar-height bitmap-height]
                           [avatar-width bitmap-width]
                           [this-tox my-tox])]
         [friend-number (friend-number my-tox key)]
         [status-msg (friend-status-msg-str my-tox friend-number)]
         [cd (contact-data name 'offline status-msg 'buddy chat-window friend-number #f)]
         [ncs (new contact-snip% [smart-list sml]
                   [style-manager cs-style]
                   [contact cd])])
    (send ncs set-status 'offline)
    (send sml insert-entry ncs)
    
    (hash-set! cur-buddies friend-number cd)
    (send chat-window set-name name)
    (send chat-window set-key key)
    (send chat-window set-friend-num friend-number)
    (send chat-window set-friend-avatar
          (if (file-exists? avatar-file)
              avatar-file
              #f))
    
    (update-contact-status friend-number 'offline)))

(define (do-add-group name number type)
  (let* ([group-window (new group-window%
                            [this-label (format "Blight - ~a" name)]
                            [this-height 600]
                            [this-width 800]
                            [this-tox my-tox]
                            [group-number number])]
         [cd (contact-data name #f "" 'group group-window number
                           (if (eq? type 'av)
                               (gen-sources 1)
                               #f))]
         [ncs (new contact-snip% [smart-list sml]
                   [style-manager cs-style]
                   [contact cd])])
    (send ncs set-status 'groupchat)
    (send sml insert-entry ncs)
    (hash-set! cur-groups number cd)))

#|(define (add-new-group name)
  (let* ([number (add-groupchat my-tox)])
    (do-add-group (format "Groupchat #~a" number) number)))|#
(define (add-new-group name)
  (let ([number (count-chatlist my-tox)])
    (do-add-group name number 'text)
    (add-groupchat! my-tox)))

(define (add-new-av-group name)
  (let ([number (count-chatlist my-tox)]
        [av-cb (λ (mtox groupnumber peernumber pcm samples channels sample-rate userdata)
                 (printf "av-cb: gnum: ~a pnum: ~a pcm: ~a samples: ~a channels: ~a~n"
                         groupnumber peernumber pcm samples channels)
                 (printf "av-cb: srate: ~a userdata: ~a~n~n" sample-rate userdata))])
    (do-add-group name number 'av)
    (add-av-groupchat my-tox av-cb)))

(define (initial-fill-sml)
  (define an-id 1)
  (for ([fn (self-friend-list-size my-tox)])
    (define-values (success err name-bytes) (friend-name my-tox fn))
    (when (false? success)
      (error 'initial-fill-sml "failed on friendname; error ~a~n" err))
    (define name-str (bytes->string/utf-8 name-bytes))
    
    (when (string=? name-str "")
      (set! name-str (format "Anonymous #~a" an-id))
      (set! an-id (add1 an-id)))
    
    (define key (friend-key my-tox fn))
    
    (create-buddy name-str key)))

(initial-fill-sml)

; panel for choice and buttons
(define panel
  (new horizontal-panel%
       [parent frame]
       [stretchable-height #f]
       [alignment (list 'right 'center)]))

; remove a friend
(define del-friend-dialog
  (new dialog%
       [label "Blight - Remove a Tox friend"]
       [style (list 'close-button)]))

(define (do-delete-friend friend-num)
  ; delete from tox friend list
  (let-values ([(success err) (friend-delete! my-tox friend-num)])
    ; error checking...
    (cond [(eq? err 'ok)
          ; save the blight data
          (blight-save-data)
          
          ; remove from list-box
          (send sml remove-entry (get-contact-snip friend-num))
          (hash-remove! cur-buddies friend-num)
          
          ; the invite list needs to be updated for
          ; the groupchat windows that still exist
          (unless (zero? (hash-count cur-groups))
            (update-invite-list))]
          [else (friend-delete-raise "friend-delete!: There was an error deleting friend ~a: ~s"
                                     friend-num err)])))

(define (delete-friend friend-number)
  (let ([mbox (message-box "Blight - Deleting Friend"
                           "Are you sure you want to delete?"
                           del-friend-dialog
                           (list 'ok-cancel))])
    (when (eq? mbox 'ok)
      (do-delete-friend friend-number))))

; remove a group
(define (do-delete-group! grp-number)
  (let* ([grp (hash-ref cur-groups grp-number)]
         [sources (contact-data-alsources grp)])
    (for-each (λ (i) (stop-source i)) sources)
    (delete-sources! sources)
    (del-groupchat! my-tox grp-number)
    (send sml remove-entry (get-group-snip grp-number))
    (hash-remove! cur-groups grp-number)))
