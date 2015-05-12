#lang racket/gui

(provide (all-defined-out))

(require json
         "config.rkt"
         "number-conversions.rkt"
         libtoxcore-racket/functions)


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

(struct exn:blight:friend-delete exn ()
  #:extra-constructor-name make-exn:blight:friend-delete
  #:transparent)

(define (friend-delete-raise msg)
  (raise (make-exn:blight:friend-delete
          msg (current-continuation-marks))))

;; FILE TRANSFERS
;(struct send-transfer-data (path contents sent) #:mutable)
;(struct receive-transfer-data (fhandle received) #:mutable)
(struct transfer-data (friend filenumber id path contents pos fhandle) #:mutable)

;(define rt (make-hash))
;(define st (make-hash))
(define transfers (make-hash))

(struct exn:blight:transfer exn ()
  #:extra-constructor-name make-exn:blight:transfer
  #:transparent)

(define (transfers-raise msg)
  (raise (make-exn:blight:transfer
          msg (current-continuation-marks))))

(define (transfers-ref key)
  (hash-ref transfers key
            (lambda ()
              (transfers-raise (format "transfers-ref: Incorrect file transfer id: ~a" key)))))

#|(define (transfer-set! transfer key value)
  (hash-set! transfer key value))

(define (transfer-del! transfer key)
  (unless (hash-has-key? rt key)
    (transfer-raise (format "transfer-del: Incorrect file transfer id: ~a" key))))|#

; shortcuts to interface with the transfers hash

(define (transfers-set! id value)
  (hash-set! transfers id value))

(define (transfers-ref-fhandle id)
  (transfer-data-fhandle (transfers-ref id)))

(define (transfers-ref-pos id)
  (transfer-data-pos (transfers-ref id)))

(define (set-transfers-pos! id pos)
  (set-transfer-data-pos! (transfers-ref id) pos))

(define (transfers-del! id)
  (unless (hash-has-key? transfers id)
    (transfers-raise (format "transfers-del!: Incorrect file transfer id: ~s" id)))
  (when (output-port? (transfers-ref-fhandle id))
    (close-output-port (transfers-ref-fhandle id)))
  (hash-remove! transfers id))

(define transfers-add!
  (λ (tox friend filenum [id #f] [path #f] [contents #f] [pos 0] [fhandle #f])
    (define-values (success err f-id) (file-id tox friend filenum))
    (hash-set! transfers f-id
               (transfer-data friend filenum f-id path contents pos fhandle))))

(define (transfers-ref-data id)
  (transfer-data-contents (transfers-ref id)))

(define (transfers-ref-path id)
  (transfer-data-path (transfers-ref id)))

(define (transfers-ref-filename id)
  (unless (hash-has-key? transfers id)
    (transfers-raise (format "transfers-ref-filename: Incorrect file transfer id: ~a" id)))
  (path->string (last (explode-path (transfers-ref-path id)))))

(define (transfers-ref-num id)
  (transfer-data-filenumber (transfers-ref id)))

(define (transfers-read-file! id)
  (define cur-transfer (transfers-ref id))
  (define cur-path (transfer-data-path cur-transfer))
  (set-transfer-data-contents! cur-transfer (file->bytes cur-path)))

#|(define (rt-ref num)
  (transfer-ref rt num))

(define (rt-ref-fhandle num)
  (receive-transfer-data-fhandle (rt-ref num)))

(define (rt-ref-received num)
  (receive-transfer-data-received (rt-ref num)))

(define (set-rt-received! num val)
  (set-receive-transfer-data-received! (rt-ref num) val))

(define (rt-del! num)
  (transfer-del! rt num))

(define (rt-add! path id)
  (transfer-set! rt id
                 (receive-transfer-data (open-output-file
                                         path
                                         #:mode 'binary
                                         #:exists 'replace)
                                        0)))

(define (st-ref num)
  (transfer-ref st num))

(define (st-ref-data num)
  (send-transfer-data-contents (transfer-ref st num)))

(define (st-ref-path num)
  (send-transfer-data-path (transfer-ref st num)))

(define (st-ref-sent num)
  (send-transfer-data-sent (transfer-ref st num)))

(define (set-st-sent! num val)
  (set-send-transfer-data-sent! (st-ref num) val))

(define (st-del! num)
  (transfer-del! st num))

(define (st-add! path id)
  (transfer-set! st id
		 (send-transfer-data path #f 0)))

(define (st-read-file! id)
  (define cur-st (st-ref id))
  (define cur-path (send-transfer-data-path cur-st))
  (set-send-transfer-data-contents! cur-st (file->bytes cur-path)))|#

(define (format-anonymous public-key)
  (format "Anonymous (~a)" (substring public-key 0 5)))

; REPL server/client stuff

(define debug-prefix (make-parameter ""))

(define blight-tcp-port 6543)

(define (print-wait msg . args)
  (apply printf msg args)
  (display " ... ")
  (flush-output))

(define (dprint-wait . args)
  (display (debug-prefix))
  (apply print-wait args))

(define (dprint-ok)
  (display (debug-prefix))
  (displayln "Ok."))

(define (dprintf fmt . args)
  (display (debug-prefix))
  (apply printf fmt args))

(define (write-data/flush data [out (current-output-port)])
  (write data out)
  (display "  " out)
  (flush-output out))

(define bytes->hex-string
  (λ (bstr)
    (define blist (bytes->list bstr))
    (define stuff (λ (item)
                    (string->list (string-upcase (dec->hex item)))))
    (list->string (flatten (map stuff blist)))))

; recursion! whee!
(define hex-string->bytes
  (λ (hexstr len)
    (cond [(zero? len) #""]
          [else
           (bytes-append
            (bytes
             (hex->dec
              (substring hexstr 0 2)))
            (hex-string->bytes (substring hexstr 2) (- len 1)))])))

#|
reusable procedure to save information to <profile>.json

1. read from <profile>.json to get the most up-to-date info
2. modify the hash
3. save the modified hash to <profile>.json

key is a symbol corresponding to the key in the hash
val is a value that corresponds to the value of the key
|#
(define blight-save-config
  (λ (key val)
    (let* ([new-input-port (open-input-file ((config-file))
                                            #:mode 'text)]
           [json (read-json new-input-port)]
           [modified-json (hash-set json key val)]
           [config-port-out (open-output-file ((config-file))
                                              #:mode 'text
                                              #:exists 'truncate/replace)])
      (display "Saving config... ")
      (json-null 'null)
      (write-json modified-json config-port-out)
      (write-json (json-null) config-port-out)
      (close-input-port new-input-port)
      (close-output-port config-port-out)
      (displayln "Done!"))))

; same as above, but for multiple saves at a time
(define-syntax blight-save-config*
  (syntax-rules ()
    ((_ k1 v1 k2 v2 ...)
     (let* ([new-input-port (open-input-file ((config-file))
                                             #:mode 'text)]
            [json (read-json new-input-port)]
            [modified-json (hash-set* json
                                      k1 v1
                                      k2 v2
                                      ...)]
            (config-port-out (open-output-file ((config-file))
                                               #:mode 'text
                                               #:exists 'truncate/replace)))
       (display "Saving config... ")
       (json-null 'null)
       (write-json modified-json config-port-out)
       (write-json (json-null) config-port-out)
       (close-input-port new-input-port)
       (close-output-port config-port-out)
       (displayln "Done!")))))
