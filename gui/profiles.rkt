#lang racket/gui
; profiles.rkt
; contains information about the profiles dialog window
(require "../config.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define profiles-box (new dialog%
                          [label "Blight - Manage Profiles"]
                          [style (list 'close-button)]
                          [height 100]
                          [width 400]))

(define profile-message (new message%
                             [parent profiles-box]
                             [label "Select a profile:"]))

(define profile-caveat (new message%
                            [parent profiles-box]
                            [label "(Profile will be selected upon program restart.)"]))

; choices for available profiles
(define profiles-choice
  (let ([profile-last (hash-ref json-info 'profile-last)])
    (new choice%
         [parent profiles-box]
         [label #f]
         [stretchable-width #t]
         [choices ((profiles))]))) ; list of available profiles

(define profiles-hpanel
  (new horizontal-panel%
       [parent profiles-box]
       [alignment '(right center)]))

(define profiles-cancel-button
  (new button%
       [parent profiles-hpanel]
       [label "Cancel"]
       [callback (λ (button event)
                   (send profiles-box show #f))]))

(define profiles-export-button
  (new button%
       [parent profiles-hpanel]
       [label "Export"]
       [callback (λ (button event)
                   (let ([path (get-directory "Blight - Export Data" ; label
                                              #f ; parent
                                              tox-path)] ; directory
                         [selection-str (send profiles-choice get-string-selection)])
                     (unless (false? path)
                       (printf "Exporting profile ~a to ~a... " selection-str path)
                       (copy-file ((data-file selection-str))
                                  (build-path path (file-name-from-path ((data-file)))))
                       (displayln "Done!"))
                     (send profiles-box show #f)))]))

; delete the selected profile
(define profiles-delete-button
  (new button%
       [parent profiles-hpanel]
       [label "Delete"]
       [callback
        (λ (button event)
          (let-values ([(mbox cbox) (message+check-box
                                     "Blight - Delete Profile" ; label
                                     "Are you certain you want to delete this profile?" ; msg
                                     "Delete History DB" ; cbox label
                                     #f ; parent
                                     '(ok-cancel stop))] ; style
                       [(selection-num) (send profiles-choice get-selection)]
                       [(selection-str) (send profiles-choice get-string-selection)])
            (when (eq? mbox 'ok)
              (printf "Deleting profile ~a... " selection-str)
              (send profiles-choice delete selection-num)
              (delete-file ((data-file selection-str)))
              (delete-file ((config-file selection-str)))
              ; if cbox is selected, also delete db-file
              (cond [(false? cbox)
                     (displayln "Done!")]
                    [else (delete-file ((db-file selection-str)))
                          (displayln "Done!")])
              (send profiles-box show #f))))]))

; Select button for preferences dialog box
(define profiles-ok-button
  (new button%
       [parent profiles-hpanel]
       [label "Select"]
       [callback (λ (button event)
                   (blight-save-config 'profile-last
                                       (send profiles-choice get-string-selection))
                   (send profiles-box show #f))]))
