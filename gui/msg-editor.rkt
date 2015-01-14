#lang racket/gui
; msg-editor.rkt
; contains common message editor functions and keymaps
(require "../utils.rkt")

(provide (all-defined-out))

(define (init-editor-keymap cw)
      (let ([km (new keymap%)])
        
        (send km add-function "insert-clipboard"
              (lambda (editor kev)
                (send editor paste)))
        
        (send km add-function "insert-primary"
              (lambda (editor kev)
                (send editor paste-x-selection)))
        
        (send km add-function "cut"
              (lambda (editor kev)
                (send editor cut)))
        
        (send km add-function "copy"
              (lambda (editor kev)
                (send editor copy)))
        
        (send km add-function "select-all"
              (lambda (editor kev)
                (send editor move-position 'end)
                (send editor extend-position 0)))
        
        (send km add-function "send-and-clear"
              (lambda (editor kev)
                (unless (string=? (send editor get-text) "")
                  (send cw do-send-message editor (send editor get-text))
                  (send editor erase)
                  (send cw set-editor-black-style editor))))
        
        (send km add-function "insert-newline"
              (lambda (editor kev)
                (send editor insert "\n")))
        
        (send km add-function "delete-backward-char"
              (lambda (editor kev)
                (send editor delete)))
        
        (send km add-function "delete-forward-char"
              (lambda (editor kev)
                (send editor delete
                      (send editor get-start-position)
                      (+ (send editor get-end-position) 1))))
        
        (send km add-function "backward-char"
              (lambda (editor kev)
                (send editor move-position 'left)))
        
        (send km add-function "backward-word"
              (lambda (editor kev)
                (send editor move-position 'left #f 'word)))

        (send km add-function "backward-kill-word"
              (lambda (editor kev)
                (let ([to (send editor get-start-position)])
                  (send editor move-position 'left #f 'word)
                  (let ([from (send editor get-start-position)])
                    (send editor delete
                          from to)))))

        (send km add-function "forward-kill-word"
              (lambda (editor kev)
                (let ([from (send editor get-start-position)])
                  (send editor move-position 'right #f 'word)
                  (let ([to (send editor get-start-position)])
                    (send editor delete
                          from to)))))

        (send km add-function "mark-char-backward"
              (lambda (editor kev)
                (let ([cur (send editor get-start-position)])
                  (send editor move-position 'left #t 'simple))))

        (send km add-function "mark-char"
              (lambda (editor kev)
                (let ([cur (send editor get-start-position)])
                  (send editor move-position 'right #t 'simple))))
        
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
        
        (send km add-function "menu"
              (λ (editor kev)
                (let ([evt (send kev get-event-type)]
                      [ecanvas (send editor get-canvas)])
                  (cond [(eq? evt 'right-up)
                         ; open the right-click menu
                         (let* ([x-mouse (send kev get-x)]
                                [y-mouse (send kev get-y)]
                                [top-frame (send ecanvas get-top-level-window)])
                           
                           (define popup
                             (new popup-menu% [title "Right Click Menu"]))
                           
                           (define copy-item
                             (new menu-item%
                                  [label "Copy"]
                                  [parent popup]
                                  [help-string "Copy this selection"]
                                  [callback (λ (l e)
                                              (send editor copy))]))
                           
                           (define cut-item
                             (new menu-item%
                                  [label "Cut"]
                                  [parent popup]
                                  [help-string "Cut this selection"]
                                  [callback (λ (l e)
                                              (send editor cut))]))
                           
                           (define paste-item
                             (new menu-item%
                                  [label "Paste"]
                                  [parent popup]
                                  [help-string "Paste from the clipboard"]
                                  [callback (λ (l e)
                                              (send editor paste))]))
                           
                           (send top-frame popup-menu popup x-mouse (+ y-mouse 100)))]))))
        
        (send km add-function "special-insert-symbol"
              (lambda (editor kev)
                (let ([key (send kev get-key-code)]
                      [control (send kev get-control-down)]
                      [shift (send kev get-shift-down)]
                      [alt (send kev get-alt-down)])
                  (cond
                    [(and (eqv? key #\\) (eq? control #t))
                     (send editor insert "\u03BB")] ; λ
                    [(and (eqv? key #\1) (eq? control #t))
                     (send editor insert "\u00A9")] ; copyright
                    [(and (eqv? key #\2) (eq? control #t))
                     (send editor insert "\u00AE")] ; registered-trademark
                    [(and (eqv? key #\3) (eq? control #t))
                     (send editor insert "\u2122")] ; trademark
                    ))))
        km))

(define (set-default-editor-bindings km)
  (send km map-function ":c:c" "copy")
  (send km map-function ":c:с" "copy") ;; russian cyrillic
  (send km map-function ":c:v" "insert-clipboard")
  (send km map-function ":c:м" "insert-clipboard") ;; russian cyrillic
  (send km map-function ":c:x" "cut")
  (send km map-function ":c:ч" "cut") ;; russian cyrillic
  (send km map-function ":c:a" "select-all")
  (send km map-function ":c:ф" "select-all") ;; russian cyrillic
  (send km map-function "~s:return" "send-and-clear")
  (send km map-function ":s:return" "insert-newline")
  (send km map-function ":numpadenter" "insert-newline")
  (send km map-function ":backspace" "delete-backward-char")
  (send km map-function ":delete" "delete-forward-char")
  (send km map-function ":left" "backward-char")
  (send km map-function ":right" "forward-char")
  (send km map-function ":c:left" "backward-word")
  (send km map-function ":c:right" "forward-word")
  (send km map-function ":c:backspace" "backward-kill-word")
  (send km map-function ":c:delete" "forward-kill-word")
  (send km map-function ":s:left" "mark-char-backward")
  (send km map-function ":s:right" "mark-char")
  (send km map-function ":up" "previous-line")
  (send km map-function ":down" "next-line")
  (send km map-function ":home" "beginning-of-buffer")
  (send km map-function ":end" "end-of-buffer")
  (send km map-function ":wheelup" "wheel-up")
  (send km map-function ":wheeldown" "wheel-down")
  (send km map-function ":rightbuttonseq" "menu")
  (send km map-function ":middlebutton" "insert-primary")
  (send km map-function ":s:insert" "insert-primary")
  (send km map-function ":c:1" "special-insert-symbol")
  (send km map-function ":c:2" "special-insert-symbol")
  (send km map-function ":c:3" "special-insert-symbol")
  (send km map-function ":c:\\" "special-insert-symbol"))
