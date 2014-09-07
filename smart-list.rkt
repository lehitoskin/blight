#lang racket
(require racket/gui)
(require mrlib/aligned-pasteboard)

;; every custom snip needs snip-class%
(define ssc%
  (class snip-class%
    (super-new)

    (define/override (read f)
      (void))

    (send this set-classname "ssc")
    (send this set-version 1)))

;; stream for iterating through smart snips
(define-struct sml-stream (cs)
  #:methods gen:stream
  [(define (stream-empty? sr)
     (eq? (sml-stream-cs sr) #f))
   (define (stream-first sr)
     (sml-stream-cs sr))
   (define (stream-rest sr)
     (sml-stream (send (sml-stream-cs sr) next)))])

(define smart-snip<%>
  (interface () get-key ss>? set-data get-data set-selected get-selected set-style-manager get-style-manager))

(struct cs-data (name status glyph status-msg))

(define smart-snip-style-manager<%>
  (interface () get-bg-color get-bg-color-sel get-text-color get-text-color-sel))

(define cs-style-manager
  (class* object% (smart-snip-style-manager<%>)

    (super-new)
    
    (define/public (get-bg-color)
      (send the-color-database find-color "White"))
    
    (define/public (get-bg-color-sel)
      (send the-color-database find-color "DarkBlue"))
    
    (define/public (get-text-color)
      (send the-color-database find-color "Black"))
    
    (define/public (get-text-color-sel)
      (send the-color-database find-color "White"))))

(define contact-snip%
  (class* snip% (smart-snip<%>)
    (super-new)
    (send this set-snipclass (make-object ssc%))
    
    (init-field smart-list style-manager snip-data)

    (define snip-text (cs-data-name snip-data))
    (define snip-glyph (cs-data-glyph snip-data))
    
    ;; selected status
    (define selected #f)

    ;; glyph 
    (define glyphw (send snip-glyph get-width))
    (define glyphh (send snip-glyph get-height))

    (define hgap 10) ;; horizontal gap
    (define selected? #f) ;; is selected
    (define selection-changed? #f)

    (define (get-snip-extent dc)
      (let-values ([(text-width text-height dist evert) (send dc get-text-extent snip-text)])
        (values (+ glyphw text-width hgap)
                (+ glyphh text-height))))

    (define (get-text-extent dc)
      (let-values ([(text-width text-height dist evert) (send dc get-text-extent snip-text)])
        (values text-width text-height)))
    
    (define/override (get-extent dc x y
                                 w h descent space lspace rspace)
      (let-values ([(tw th dist evert) (send dc get-text-extent snip-text)])
        (when w
          (set-box! w (+ glyphw tw hgap)))
        (when h
          (set-box! h (+ glyphh th)))
        (when descent 
          (set-box! descent 0))
        (when space
          (set-box! space 0))
        (when lspace
          (set-box! lspace 0))
        (when rspace
          (set-box! rspace 0)))
      (void))

    (define/public (get-style-manager)
      style-manager)

    (define/public (set-style-manager mgr)
      (set! style-manager mgr))

    (define/public (set-selected s?)
      (printf "selection changed for ~a: ~a\n" snip-text s?)
      (set! selected? s?))

    (define/public (get-selected)
      selected?)

    (define/public (get-key)
      snip-text)

    (define/public (set-data nd)
      (set! snip-data nd))

    (define/public (get-data)
      snip-data)

    (define/public (ss>? sn)
      (let* ([sd (send sn get-data)]
             [status1 (cs-data-status snip-data)]
             [status2 (cs-data-status sd)]
             [name1 (cs-data-name snip-data)]
             [name2 (cs-data-name sd)])
        (cond
         [(> status1 status2) #t]
         [(eq? status1 status2) (string<? name1 name2)]
         [else #f])))

    (define snip-text-fg-sel (send style-manager get-text-color-sel))
    (define snip-text-bg-sel (send style-manager get-bg-color-sel))

    (define snip-text-fg (send style-manager get-text-color))
    (define snip-text-bg (send style-manager get-bg-color))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (let-values ([(snip-width snip-height) (get-snip-extent dc)]
                   [(text-width text-height) (get-text-extent dc)])
        
        (if selected?
            (begin
              (send dc set-text-foreground snip-text-fg-sel)
              (let-values ([(snip-width snip-height) (get-snip-extent dc)])
                (send dc set-brush snip-text-bg-sel 'solid)
                (send dc draw-rectangle (+ x glyphw hgap) y text-width text-height)))
            
            (send dc set-text-foreground snip-text-fg))

        (send dc draw-bitmap snip-glyph x y 'xor)
        (send dc draw-text snip-text (+ x glyphw hgap) y)))
    
    (define/override (get-flags)
      (list 'handles-events 'handles-all-mouse-events))

    (define/override (on-char dc x y editorx editory event)
      (printf "on char: ~a\n" event))

    (define/override (on-event dc x y editorx editory event)
      (when (send event button-up? 'left)
        (send smart-list set-selected this)
              (printf "on event: ~a\n" event)))))

  (define smart-list%
    (class vertical-pasteboard%
      (define cur-sel #f) ;; currently selected entry
      (define strings #f) ;; TODO: usernames prefix tree
      (define snip-hash (make-hash))

      (super-new)
      (send this set-selection-visible #f)

      ; TODO
      (define/public (find-string)
        #f
        )

      (define/public (get-number)
        1
        )

      (define/public (set-string n label)
        
        (void)
        )

      (define/public (get-snip-stream)
        (let ([fs (send this find-first-snip)])
          (sml-stream  fs)))

      (define/public (get-entry key)
        (hash-ref snip-hash key))

      (define/augment (on-select snip on?)
        (send snip set-selected on?))

      ;; (define/public (sort)
        
      ;;   )

      ;; (define/public (enumerate-snips func)
      ;;   (let loop ([ns (send this find-first-snip)])
      ;;     (when (xor ns #f)
      ;;       (func ns)  
      ;;       (loop (send ns next)))))

      ;; (define/public (set-selected entry)
      ;;   (printf "setting selected: ~a\n" (send entry get-ky)))
      
      (define/public (insert-entry ns)
        
        (let ([key (send ns get-key)])
          (begin
            ;; (printf "inserting: ~a\n" key)
            (if (hash-empty? snip-hash) 
                (begin
                  
                  (send this insert ns)) ;; list is empty, just insert
                (let ([first-less
                       ;; find the first element less than snip being inserted                     
                       (for/first ([el (get-snip-stream)]
                                   #:when (send ns ss>? el))
                         
                         el)])
                  (begin
                    ;; (printf "first less: ~a\n" first-less)
                    (send this insert ns)
                    (if (eq? first-less #f)
                        ;; no such, insert to the end
                        (send this set-after ns #f)
                        ;; insert before the first-less snip
                        (send this set-before ns first-less)))))
        (hash-set! snip-hash key ns))))))

(define (init-smart-list-keymap)
  (let ([km (new keymap%)])
    (send km add-function "select-next"
              (lambda (pb kev)
                (printf "next: ~a\n" kev)))
    km))

(define (init-default-smartlist-keymap km)
  (send km map-function "n" "select-next"))

(define (run)

 (let* ([frame (new frame% [label "Smart-list demo"]
                    [width 640]
                    [height 480])]
        [pb (new smart-list%)]
        [ec (new aligned-editor-canvas% [parent frame] [editor pb])]
        [ss1 (new string-snip%)]
        [ss2 (new string-snip%)]
        [bmp (make-object bitmap% "icons/available.png")]
        [cs-style (new cs-style-manager)]

        [ss4 (new contact-snip% [smart-list pb] [style-manager cs-style] [snip-data (cs-data "foo" 1 bmp "status1")])]
        [ss5 (new contact-snip% [smart-list pb] [style-manager cs-style] [snip-data (cs-data "bar" 1 bmp "status2")])]
        [ss6 (new contact-snip% [smart-list pb] [style-manager cs-style] [snip-data (cs-data "baz" 1 bmp "status3")])]
        [ss7 (new contact-snip% [smart-list pb] [style-manager cs-style] [snip-data (cs-data "qux" 5 bmp "status4")])]
        [km (init-smart-list-keymap)])

   (send pb insert-entry ss4)
   (send pb insert-entry ss5)
   (send pb insert-entry ss6)
   (send pb insert-entry ss7)

   ;; (send pb set-caret-owner ss4)

   (init-default-smartlist-keymap km)
   (send pb set-keymap km)

   ;; (send pb lower ss4)
   ;; (send pb set-before ss5 #f)

   (for ([x (send pb get-snip-stream)])
     (printf "snip: ~a\n" (send x get-key)))

   
   (send frame show #t)
   (sleep/yield 1)))
