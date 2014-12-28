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
(struct send-transfer-data (path contents) #:mutable)

(define rt (make-hash))
(define st (make-hash))

(struct	 exn:blight:rtransfer exn ()
         #:extra-constructor-name make-exn:blight:rtransfer
         #:transparent)

(define (transfer-raise msg)
  (raise (make-exn:blight:rtransfer
          msg (current-continuation-marks))))

(define (transfer-ref transfer key)
  (hash-ref transfer key
            (lambda ()
              (transfer-raise (format "transfer-ref: Incorrect file transfer id: ~a" key)))))

(define (transfer-set! transfer key value)
  (hash-set! transfer key value))

(define (transfer-del! transfer key)
  (unless (hash-has-key? rt key)
    (transfer-raise (format "transfer-del: Incorrect file transfer id: ~a" key))))

(define (rt-ref num)
  (transfer-ref rt num))

(define (rt-del! num)
  (transfer-del! rt num))

(define (rt-add! path id)
  (transfer-set! rt id
             (open-output-file
              path
              #:mode 'binary
              #:exists 'replace)))

(define (st-ref num)
  (transfer-ref st num))

(define (st-ref-data num)
  (send-transfer-data-contents (transfer-ref st num)))

(define (st-ref-path num)
  (send-transfer-data-path (transfer-ref st num)))

(define (st-del! num)
  (transfer-del! st num))

(define (st-add! path id)
  (transfer-set! st id
		 (send-transfer-data path #f)))

(define (st-read-file! id)
  (define cur-st (st-ref id))
  (define cur-path (send-transfer-data-path cur-st))
  (set-send-transfer-data-contents! cur-st (file->bytes cur-path)))

(define (format-anonymous public-key)
  (format "Anonymous (~a)" (substring public-key 0 5)))

; REPL server/client stuff

(define debug-prefix (make-parameter ""))

(define blight-tcp-port 7654)

(define (print-wait msg . args)
  (apply printf msg args)
  (display " ... ")
  (flush-output))

(define (dprint-wait . args)
  (display (debug-prefix)))

(define dprint-ok
  (λ ()
    (display (debug-prefix))
    (displayln "Ok.")))

(define (dprintf fmt . args)
  (display (debug-prefix))
  (apply printf fmt args))

(define (write-data/flush data [out (current-output-port)])
  (write data out)
  (display "  " out)
  (flush-output out))
