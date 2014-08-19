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

