#lang racket/gui
; msg-history.rkt
; contains common message history functions and keymaps

(require "utils.rkt")
(provide (all-defined-out))

(define (init-messages-keymap cw)
      (let ([km (new keymap%)])
        (send km add-function "copy"
              (lambda (editor kev)
                (send editor copy)))
        
        (send km add-function "backward-char"
              (lambda (editor kev)
                (send editor move-position 'left)))
        
        (send km add-function "select-all"
              (lambda (editor kev)
                (send editor move-position 'end)
                (send editor extend-position 0)))
        
        (send km add-function "backward-word"
              (lambda (editor kev)
                (send editor move-position 'left #f 'word)))
        
        (send km add-function "forward-char"
              (lambda (editor kev)
                (send editor move-position 'right)))
        
        (send km add-function "forward-word"
              (lambda (editor kev)
                (send editor move-position 'right #f 'word)))
        
        (send km add-function "previous-line"
              (lambda (editor kev)
                (send editor move-position 'up)))
        
        (send km add-function "next-line"
              (lambda (editor kev)
                (send editor move-position 'down)))
        
        (send km add-function "beginning-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'home)))
        
        (send km add-function "end-of-buffer"
              (lambda (editor kev)
                (send editor move-position 'end)))
        
        (send km add-function "wheel-up"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'up))
                 (send (send editor get-canvas) wheel-step))))
        
        (send km add-function "wheel-down"
              (lambda (editor kev)
                (repeat
                 (λ () (send editor move-position 'down))
                 (send (send editor get-canvas) wheel-step))))
        km))

(define (set-default-messages-bindings km)
      (send km map-function ":c:c" "copy")
      (send km map-function ":c:с" "copy") ;; russian cyrillic
      
      (send km map-function ":c:a" "select-all")
      (send km map-function ":c:ф" "select-all") ;; russian cyrillic
      
      (send km map-function ":left" "backward-char")
      (send km map-function ":right" "forward-char")
      (send km map-function ":c:left" "backward-word")
      (send km map-function ":c:right" "forward-word")
      (send km map-function ":up" "previous-line")
      (send km map-function ":down" "next-line")
      (send km map-function ":home" "beginning-of-buffer")
      (send km map-function ":end" "end-of-buffer")
      
      (send km map-function ":wheelup" "wheel-up")
      (send km map-function ":wheeldown" "wheel-down"))

; normal black
(define color-black (make-object color% "black"))
; a darker green than "green", which looks nicer on a white background
(define color-green (make-object color% 35 135 0))

(define black-style (make-object style-delta% 'change-size 10))
; make this style black
(void (send black-style set-delta-foreground color-black))

(define green-style (make-object style-delta% 'change-size 10))
; make this style green, for the greentext
(void (send green-style set-delta-foreground color-green))


; procedure to imply things
(define imply
  (λ (editor msg)
    (send editor change-style green-style)
    (send editor insert (string-append msg "\n"))
    (send editor change-style black-style)))

; if the current cursor position is not at the end, move there
(define (save-move-cursor editor)
  (when (not (= (send editor get-start-position)
                (send editor get-end-position)))
    (send editor move-position 'end)))

; procedure to imply things
(define message-history%
  (class object%
    (init-field editor)

    (super-new)

    (define/public (send-file-recv-error msg)
      (save-move-cursor editor)
      (send editor insert (format "\n*** File transfer error: ~a ***\n\n" msg)))

    (define/public (begin-send-file path time)
      (save-move-cursor editor)
      (send editor insert (format "\n*** Starting transfer: ~a ***\n\n" path)))

    (define/public (end-send-file path time)
      (save-move-cursor editor)
      (send editor insert (format "\n*** Sent: ~a ***\n\n" path)))

    (define/public (begin-recv-file path time)
      (save-move-cursor editor)
      (send editor insert (format "\n*** Starting download to ~a ***\n\n" path)))
    
    (define/public (end-recv-file time)
      (save-move-cursor editor)
      (send editor insert (format "\n*** Download complete ***\n\n")))
    
    (define/public (add-recv-action action from time)
      (save-move-cursor editor)
      
      (send editor insert
            (string-append "** [" time "] " from " " action "\n")))
    
    (define/public (add-recv-message message from time)
      (save-move-cursor editor)
      
      (send editor insert
            (string-append "[" time "] " from ": "))
      
      (if (string=? (substring message 0 1) ">")
          (imply editor message)
          (send editor insert (string-append message "\n"))))))

