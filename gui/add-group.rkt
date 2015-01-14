#lang racket/gui
; add-group.rkt
; contains code that adds a groupchat via a little dialog
(require libtoxcore-racket/functions
         "frame.rkt"
         "friend-list.rkt"
         "smart-list.rkt"
         "../tox.rkt")

(provide (all-defined-out))

(define add-group-button
  (new button%
       [parent panel]
       [label "Add group"]
       [callback (λ (button event)
                   ; open a dialogue to optionally name the groupchat
                   (define add-group-frame (new frame% [label "Add Group"]))
                   
                   (define add-group-message
                     (new message%
                          [label "Please enter a(n optional) Group Chat name"]
                          [parent add-group-frame]))
                   
                   (define add-group-tfield
                     (new text-field%
                          [label "Group Chat name: "]
                          [parent add-group-frame]
                          [callback (λ (l e)
                                      (when (eq? (send e get-event-type) 'text-field-enter)
                                        (let* ([gcount (hash-count cur-groups)]
                                               [str (send l get-value)]
                                               [bstr (string->bytes/utf-8 str)]
                                               [no-name #"Group Chat"])
                                          ; no group name supplied, go with defaults
                                          (cond [(string=? str "")
                                                 (if (send add-group-av-check get-value)
                                                     (add-new-av-group
                                                      (format "Groupchat #~a" gcount))
                                                     (add-new-group
                                                      (format "Groupchat #~a" gcount)))
                                                 (group-set-title! my-tox gcount no-name)
                                                 (send l set-value "")
                                                 (send add-group-frame show #f)]
                                                ; group name supplied, use that
                                                [else
                                                 ; add group with number and name
                                                 (if (send add-group-av-check get-value)
                                                     (add-new-av-group
                                                      (format "Groupchat #~a" gcount))
                                                     (add-new-group
                                                      (format "Groupchat #~a" gcount)))
                                                 (define window (contact-data-window
                                                                 (hash-ref cur-groups gcount)))
                                                 ; set the group title we chose
                                                 (group-set-title! my-tox
                                                                  gcount
                                                                  bstr)
                                                 (send window set-new-label
                                                       (format "Blight - Groupchat #~a: ~a"
                                                               gcount str))
                                                 (send (get-group-snip gcount)
                                                       set-status-msg str)
                                                 (send l set-value "")
                                                 (send add-group-frame show #f)]))))]))
                   
                   (define add-group-av-check
                     (new check-box%
                          [parent add-group-frame]
                          [label "Enable Audio"]
                          [value #f]))
                   
                   ; TODO: tick box for audio capabilities
                   
                   (define add-group-hpanel (new horizontal-panel%
                                                 [parent add-group-frame]
                                                 [alignment '(right center)]))
                   
                   (define add-group-cancel-button
                     (new button%
                          [parent add-group-hpanel]
                          [label "Cancel"]
                          [callback (λ (button event)
                                      (send add-group-tfield set-value "")
                                      (send add-group-frame show #f))]))
                   
                   (define add-group-ok-button
                     (new button%
                          [parent add-group-hpanel]
                          [label "&OK"]
                          [callback
                           (λ (button event)
                             ; add the group
                             (let* ([str (send add-group-tfield get-value)]
                                    [bstr (string->bytes/utf-8 str)]
                                    [gcount (hash-count cur-groups)]
                                    [no-name #"Group Chat"])
                               ; no group name supplied, go with defaults
                               (cond [(string=? str "")
                                      (if (send add-group-av-check get-value)
                                          (add-new-av-group (format "Groupchat #~a" gcount))
                                          (add-new-group (format "Groupchat #~a" gcount)))
                                      (group-set-title! my-tox gcount no-name)
                                      (send add-group-tfield set-value "")
                                      (send add-group-frame show #f)]
                                     ; group name supplied, use that
                                     [else
                                      ; add group with number and name
                                      (if (send add-group-av-check get-value)
                                          (add-new-av-group (format "Groupchat #~a" gcount))
                                          (add-new-group (format "Groupchat #~a" gcount)))
                                      (define window
                                        (contact-data-window (hash-ref cur-groups gcount)))
                                      ; set the group title we chose
                                      (group-set-title! my-tox
                                                       gcount
                                                       bstr)
                                      (send (get-group-snip gcount)
                                            set-status-msg str)
                                      (send add-group-tfield set-value "")
                                      (send add-group-frame show #f)])))]))
                   
                   (send add-group-frame show #t))]))

(define del-group-button
  (new button%
       [parent panel]
       [label "Del group"]
       [callback (λ (button event)
                   (send sml call-delete-entry-cb (send sml get-selection-cd)))]))
