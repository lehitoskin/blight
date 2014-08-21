#lang racket

(provide (all-defined-out))

(require racket/gui)


(define (repeat proc times)
  (cond [(zero? times) #t]
        [else (proc) (repeat proc (- times 1))]))

(define error-dialog (new dialog%
                              [label "Blight Oops!"]
                              [style (list 'close-button)]))

(define error-text (new text%
                            [line-spacing 1.0]
                            [auto-wrap #t]))
  
(define error-editor-canvas
  (new editor-canvas%
       [parent error-dialog]
       [min-height 600]
       [min-width 800]
       [vert-margin 10]
       [editor error-text]
       [style (list 'control-border 'no-hscroll
                    'auto-vscroll 'no-focus)]))

(define error-ok
  (new button%
       [parent error-dialog]
       [label "&Continue"]
       [callback (λ (button event)
                    (send error-dialog show #f)
                    'continue
                    )]))

(define error-quit
  (new button%
       [parent error-dialog]
       [label "&Quit Blight"]
       [callback (λ (button event)
                    (send error-dialog show #f)
                    'quit
                    )]))

(define (show-error-unhandled-exn unexn)
  (send error-text clear)
  (send error-text insert "This is a bug!\n\n")

  (let ([ostr (open-output-string)])
    
    (parameterize ([current-error-port ostr])
      ((error-display-handler) "Unhandled exception in blight:" unexn)
      (send error-text insert
          (format "~a\n" (get-output-string ostr))))

    ((error-display-handler) "Unhandled exception in blight:" unexn))

  (send error-dialog show #t))

