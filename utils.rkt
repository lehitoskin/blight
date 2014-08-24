#lang racket

(provide (all-defined-out))

(require racket/gui
         "helpers.rkt")


;;; TODO: use structure type properties here
(define-struct tox-ctx (my-tox my-id-bytes  f-cleanup)
  #:transparent)

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


(define (show-error-unhandled-exn unexn ctx)
  (define error-ok
    (new button%
         [parent error-dialog]
         [label "&Continue"]
         [callback (λ (button event)
                      (send error-dialog show #f)
                      )]))

  (define error-quit
    (new button%
         [parent error-dialog]
         [label "&Quit Blight"]
         [callback (λ (button event)
                      (send error-dialog show #f)
                      ((tox-ctx-f-cleanup ctx))
                      (exit)
                      )]))

  (let loop ()
      (when (send error-text locked-for-write?)
        (eprintf "Exception handler: waiting for error-text to unlock...\n")
        (sleep 1)
        (loop)))

  (send error-text begin-edit-sequence #f #f)
  
  (send error-text clear)
  (send error-text insert "This is a bug. Please report.\n\n")

  (let ([ostr (open-output-string)])

    ((error-display-handler) (exn-message unexn) unexn)
    (flush-output (current-error-port))

    (parameterize ([current-error-port ostr])
      ((error-display-handler) (exn-message unexn) unexn)
      (send error-text insert
          (format "~a\n" (get-output-string ostr)))))

  (send error-text end-edit-sequence)

  (send error-dialog show #t))

;; FILE TRANSFERS
(define rt null)

(struct	 exn:blight:rtransfer exn ()
         #:extra-constructor-name make-exn:blight:rtransfer
         #:transparent)

(define (rt-raise msg)
  (raise (make-exn:blight:rtransfer
          msg (current-continuation-marks))))

(define (rt-ref num)
  (when (<= (length rt) num)
    (rt-raise (format "Incorrect file transfer id: ~a (total: ~a)" num (length rt))))

  (car (list-ref rt num)))

(define (rt-del! num)
  (when (<= (length rt) num)
    (rt-raise (format "Incorrect file transfer id: ~a (total: ~a)" num (length rt))))
  
  (set! rt (delnode rt num)))

(define (rt-add! path)
  (set! rt
        (append rt
                (list (cons (open-output-file
                             path
                             #:mode 'binary
                             #:exists 'replace)
                            'placeholder)))))

