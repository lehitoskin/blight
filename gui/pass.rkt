#lang racket/gui
; pass.rkt

(provide (all-defined-out))

; parameterized callback
(define pass-callback (make-parameter (λ () #f)))

(define pass-dialog (new dialog%
                         [label "Blight - Enter Passphrase"]
                         [height 50]
                         [width 400]
                         [style (list 'close-button)]))

(define pass-tfield
  (new text-field%
       [label "Enter Passphrase: "]
       [parent pass-dialog]
       [style '(single password)]
       [callback (λ (l e)
                   (when (and (eq? (send e get-event-type) 'text-field-enter)
                              (not (string=? (send l get-value) "")))
                     ((pass-callback))))]))

(define pass-hpanel
  (new horizontal-panel%
       [parent pass-dialog]
       [alignment '(right center)]))

(define pass-cancel-button
  (new button%
       [label "Cancel"]
       [parent pass-hpanel]
       [callback (λ (button event)
                   (send pass-dialog show #f)
                   (send pass-tfield set-value "")
                   (exit))]))

(define pass-ok-button
  (new button%
       [label "OK"]
       [parent pass-hpanel]
       [callback (λ (button event)
                   ((pass-callback)))]))
