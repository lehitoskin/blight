#lang racket
(require racket/gui)
(require mrlib/aligned-pasteboard)
(require racket/contract)

(provide (all-defined-out))

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
  (interface () get-key set-key ss>? set-data get-data set-selected get-selected set-style-manager get-style-manager))

(struct cs-data (name status-msg) #:mutable)
(struct contact-data (name status status-msg type window tox-num) #:transparent #:mutable)

(define smart-snip-style-manager<%>
  (interface () get-bg-color get-bg-color-sel get-text-color get-text-color-sel))

(define status/c
  (symbols 'available 'offline 'away 'busy 'groupchat))

(define (init-cs-keymap)
  (let ([km (new keymap%)])
    (send km add-function "open-dialogue"
          (lambda (cur-snp kev)
            (let* ([sel-cd (get-field contact cur-snp)])
              (send (contact-data-window sel-cd) show #t))
            (printf "open dialogue\n")))

    (send km add-function "select"
          (lambda (cur-snp kev)
            (send (get-field smart-list cur-snp) set-selected cur-snp)
            (printf "~a selected\n" (send cur-snp get-key))))
    km))

(define (init-default-cs-keymap km)
  (send km map-function ":leftbuttondouble" "open-dialogue")
  (send km map-function ":leftbutton" "select"))

(define cs-style-manager
  (class* object% (smart-snip-style-manager<%>)

    (super-new)

    (define km (init-cs-keymap))
    
    (init-default-cs-keymap km)

    (define/public (get-km) km)
    
    (define status-glyphs
      (make-hash
       (list (cons 'offline (make-object bitmap% "icons/offline.png"))
             (cons 'busy (make-object bitmap% "icons/busy.png"))
             (cons 'away (make-object bitmap% "icons/away.png"))
             (cons 'groupchat (make-object bitmap% "icons/groupchat.png"))
             (cons 'available (make-object bitmap% "icons/available.png")))))

    (define/public (get-text-font)
      (make-font #:size 12))

    (define/public (get-stext-font)
      (make-font #:size 8))

    (define/public (get-glyph status)
      (hash-ref status-glyphs status))
    
    (define/public (get-bg-color)
      (send the-color-database find-color "White"))
    
    (define/public (get-bg-color-sel)
      (send the-color-database find-color "DarkBlue"))
    
    (define/public (get-text-color)
      (send the-color-database find-color "Black"))
    
    (define/public (get-text-color-sel)
      (send the-color-database find-color "White"))

    (define/public (get-stext-color-sel)
      (send the-color-database find-color "Gray"))
    
    (define/public (get-stext-color)
      (send the-color-database find-color "Gray"))))

;; status priority table
(define sp (make-hash
            (list (cons 'groupchat 0)
                  (cons 'offline 1)
                  (cons 'busy 2)
                  (cons 'away 3)
                  (cons 'available 4))))

(define/contract (status>? st1 st2)
  (-> status/c status/c boolean?)
  ;; (printf "status> ~a ~a : ~a\n" st1 st2 (> (hash-ref sp st1) (hash-ref sp st2)))
  (> (hash-ref sp st1) (hash-ref sp st2)))

(define/contract (status=? st1 st2)
  (-> status/c status/c boolean?)
  (eq? (hash-ref sp st1) (hash-ref sp st2)))


(define/contract contact-snip%
  (class/c [set-status (->m status/c any)]
           [get-status (->m status/c)]
           [set-status-msg (->m string? any)]
           [get-status-msg (->m string?)])

  (class* snip% (smart-snip<%> stretchable-snip<%>)
    (super-new)
    (send this set-snipclass (make-object ssc%))
    
    (init-field smart-list style-manager contact)

    (define snip-height 0)
    (define snip-width 0)
    
    (define/public (get-aligned-min-height)
      (let-values ([(tw th) (get-text-extent (send smart-list get-dc))])
        (max glyphh th)))
    
    (define/public (get-aligned-min-width)
      (let-values ([(tw th) (get-text-extent (send smart-list get-dc))])
        (+ glyphw tw icon-name-hgap)))
    
    (define/public (stretchable-height)
      #f)

    (define/public (stretchable-width)
      #t)

    (define/public (stretch w h)
      (set! snip-width w)
      (set! snip-height h)
      (send smart-list resized this #t))

    (define contact-status #f)

    ;; glyph 
    (define snip-glyph #f)
    (define glyphw #f)
    (define glyphh #f)
    (set-status 'offline)
    
    ;; selected status
    (define selected #f)

    (define icon-name-hgap 10) ;; horizontal gap
    (define name-status-hgap 10) ;; horizontal gap
    
    (define selected? #f) ;; is selected
    (define selection-changed? #f)

    (define/public (get-status-msg)
      (contact-data-status-msg contact))
    
    (define/public (set-status-msg new-status-msg)
      (set-contact-data-status-msg! contact new-status-msg))
    
    (define/public (set-status new-status [status-msg #f])
      (set! snip-glyph (send style-manager get-glyph new-status))
      (set! glyphw (send snip-glyph get-width))
      (set! glyphh (send snip-glyph get-height))
      (set! contact-status new-status)
      (send smart-list update-entry this)

      (set-contact-data-status! contact new-status))

    (define/public (get-status)
      contact-status)

    (define (get-stext-extent dc)
      (send dc set-font snip-stext-font)
      
      (let-values ([(text-width text-height dist evert)
                    (send dc get-text-extent (contact-data-status-msg contact))])
        ;; (printf "text extent (~a) = ~a x ~a\n" (contact-data-name contact) text-width text-height)
        (values text-width text-height)))
    
    (define (get-text-extent dc)
      (send dc set-font snip-text-font)
      
      (let-values ([(text-width text-height dist evert)
                    (send dc get-text-extent (contact-data-name contact))])
        ;; (printf "text extent (~a) = ~a x ~a\n" (contact-data-name contact) text-width text-height)
        (values text-width text-height)))

    (define (get-snip-extent dc)
      (let-values ([(text-width text-height) (get-text-extent dc)]
                   [(sml-w sml-h) (values (box (void)) (box (void)))])
        (send smart-list get-view-size sml-w sml-h)
        (values snip-width snip-height)))

    
    (define/override (get-extent dc x y
                                 w h descent space lspace rspace)
      (let-values ([(tw th) (get-text-extent dc)])

        (when w
          (set-box! w snip-width))
        (when h
          (set-box! h snip-height))
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
      ;; (printf "selection changed for ~a: ~a\n" (contact-data-name contact) s?)
      (set! selected? s?))

    (define/public (get-selected)
      selected?)

    (define/public (get-key)
      (contact-data-name contact))

    (define/public (set-key key)
      (set-contact-data-name! contact key))

    (define/public (set-data nd)
      (set! contact nd))

    (define/public (get-data)
      contact)

    (define/public (get-cd)
      contact)
    
    (define/public (ss>? sn)
      (let* ([sd (send sn get-data)]
             [sn-status (send sn get-status) ]
             [name1 (contact-data-name contact)]
             [name2 (contact-data-name sd)])
        (begin
          ;; (printf "~a vs ~a: " name1 name2)
          (cond
           [(status>? contact-status sn-status) #t]
           [(status=? contact-status sn-status) (string<? name1 name2)]
           [else #f]))))
    
    (define snip-text-fg-sel (send style-manager get-text-color-sel))
    (define snip-stext-fg-sel (send style-manager get-stext-color-sel))
    
    (define snip-text-bg-sel (send style-manager get-bg-color-sel))

    (define snip-text-fg (send style-manager get-text-color))
    (define snip-text-bg (send style-manager get-bg-color))

    (define snip-stext-fg (send style-manager get-stext-color))

    (define snip-stext-font (send style-manager get-stext-font))
    (define snip-text-font (send style-manager get-text-font))
    
    (define draw-status? #t)

    (define/public (set-draw-status! val)
      (set! draw-status? val))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (let*-values ([(snip-width snip-height) (get-snip-extent dc)]
                   [(text-width text-height) (get-text-extent dc)]
                   [(stext-width stext-height) (get-stext-extent dc)]
                   [(stext-y) (+ y text-height (- stext-height))])
        ;; (printf "snip extent (~a) = ~a x ~a\n" (contact-data-name contact) snip-width snip-height)
        (if selected?
            (begin
              (let-values ([(snip-width snip-height) (get-snip-extent dc)])
                (send dc set-brush snip-text-bg-sel 'solid)
                (send dc draw-rectangle (+ x glyphw icon-name-hgap)
                      y snip-width snip-height))

              (send dc set-font snip-text-font)
              (send dc set-text-foreground snip-text-fg-sel)
              (send dc draw-text (contact-data-name contact)
                    (+ x glyphw icon-name-hgap) y)

              (when draw-status?
                (send dc set-text-foreground snip-stext-fg-sel)
                (send dc set-font snip-stext-font)
                (send dc draw-text (contact-data-status-msg contact)
                      (+ x glyphw icon-name-hgap text-width name-status-hgap) stext-y)))
            (begin
              (send dc set-font snip-text-font)              
              (send dc set-text-foreground snip-text-fg)
              (send dc draw-text (contact-data-name contact)
                    (+ x glyphw icon-name-hgap) y)

              (when draw-status?
                (send dc set-font snip-stext-font)              
                (send dc set-text-foreground snip-stext-fg)
                (send dc draw-text (contact-data-status-msg contact)
                      (+ x glyphw icon-name-hgap text-width name-status-hgap) stext-y))))

        (send dc draw-bitmap snip-glyph x y 'xor)))

    (define/override (get-flags)
      (list 'handles-events 'handles-all-mouse-events))

    (define/override (on-event dc x y editorx editory event)
      (let ([cs-km (send style-manager get-km)])
        (send cs-km handle-mouse-event this event)))))

  (define smart-list%
    (class vertical-pasteboard%
      (define cur-sel #f) ;; currently selected entry
      (define strings #f) ;; TODO: usernames prefix tree
      (define snip-hash (make-hash))

      (super-new)
      (send this set-selection-visible #f)

      (define delete-entry-cb (void))

      ; TODO
      (define/public (get-entry-by-key key)
        (hash-ref snip-hash key))

      (define/public (get-number)
        1)

      (define/public (set-string n label)
        (void))

      (define/public (set-delete-entry-cb fun)
        (set! delete-entry-cb fun))
      
      (define/public (call-delete-entry-cb cd)
        (delete-entry-cb cd))

      (define/public (get-snip-stream)
        (let ([fs (send this find-first-snip)])
          (sml-stream  fs)))

      (define/public (get-entry key)
        (hash-ref snip-hash key))

      (define/augment (on-select snip on?)
        (send snip set-selected on?))

      (define/override (on-default-char event)
        (printf "on char: ~a\n" event))

      (define/public (update-entry ns)
        (let ([key (send ns get-key)])
          (begin

            (let ([first-less
                   ;; find the first element less than snip
                   (for/first ([el (get-snip-stream)]
                               #:when (send ns ss>? el))
                     
                     el)])
              (begin
                (if (eq? first-less #f)
                    ;; no such, insert to the end
                    (send this set-after ns #f)
                    ;; insert before the first-less snip
                    (send this set-before ns first-less)))))))


      (define/public (remove-entry sn)
        (let ([name (send sn get-key)])
          (hash-remove! snip-hash name)
          (send this remove sn)))

      (define (reset-entry sn)
          (remove-entry sn)
          (insert-entry sn))
      
      (define/public (rename-entry sn newname)
        (let ([oldname (send sn get-key)])
          (printf "renaming ~a -> ~a\n" oldname newname)
          (send sn set-key newname)
          (reset-entry sn)))

      (define/public (get-selection)
        (send this find-next-selected-snip #f))

      (define/public (get-selection-cd)
        (define sel (get-selection))
        (if sel (send sel get-cd)
            #t))

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
            (let* ([sel (send pb find-next-selected-snip #f)]
                   [nsel (if sel (send sel next) #f)])
              (when nsel
                (send pb set-selected nsel)))
            (printf "next: ~a\n" kev)))

    (send km add-function "open-dialogue"
          (lambda (pb kev)
            (let* ([sel (send pb find-next-selected-snip #f)]
                  [sel-cd (get-field contact sel)])
              (send (contact-data-window sel-cd) show #t))
            (printf "open dialogue\n")))

    (send km add-function "delete-entry"
          (lambda (pb kev)
            (let* ([sel (send pb find-next-selected-snip #f)]
                   [sel-cd (get-field contact sel)])
              (send pb call-delete-entry-cb sel-cd))
            (printf "deleted buddy\n")))
    
    (send km add-function "select-previous"
          (lambda (pb kev)
            (let* ([sel (send pb find-next-selected-snip #f)]
                   [nsel (if sel (send sel previous) #f)])
              (when nsel
                (send pb set-selected nsel)))
            (printf "next: ~a\n" kev)))
    km))

(define (init-default-smartlist-keymap km)
  (send km map-function ":down" "select-next")
  (send km map-function ":up" "select-previous")
  (send km map-function ":space" "open-dialogue")
  (send km map-function ":delete" "delete-entry"))

(define (smart-list-test-run)

 (let* ([frame (new frame% [label "Smart-list demo"]
                    [width 640]
                    [height 480])]
        [pb (new smart-list%)]
        [ec (new aligned-editor-canvas% [parent frame] [editor pb] [style '(list 'no-hscroll)])]
        [ss1 (new string-snip%)]
        [ss2 (new string-snip%)]
        [bmp (make-object bitmap% "icons/available.png")]

        [cs-style (new cs-style-manager)]

        [ss4 (new contact-snip% [smart-list pb] [style-manager cs-style] [contact (contact-data "foo"  "status1")] [contact ""])]
        [ss5 (new contact-snip% [smart-list pb] [style-manager cs-style] [contact (contact-data "bar"  "status2")] [contact ""])]
        [ss6 (new contact-snip% [smart-list pb] [style-manager cs-style] [contact (contact-data "baz"  "status3")] [contact ""])]
        [ss7 (new contact-snip% [smart-list pb] [style-manager cs-style] [contact (contact-data "qux"  "status4")][contact ""])]
        [grp (new contact-snip% [smart-list pb] [style-manager cs-style] [contact (contact-data "Groupchat #0"  "status4")]  [contact ""])]
        [km (init-smart-list-keymap)])

   (send pb insert-entry ss4)
   (send pb insert-entry ss5)
   (send pb insert-entry ss6)
   (send pb insert-entry ss7)
   (send pb insert-entry grp)

   (send ss7 set-status 'available)
   (printf "busy for baz\n")
   (send ss6 set-status 'busy)
   (printf "busy for qux\n")   
   (send ss5 set-status 'away)

   (send pb rename-entry ss5 "basdfsadfasdf")

   (send grp set-status 'groupchat)

   (init-default-smartlist-keymap km)
   (send pb set-keymap km)

   ;; (send pb lower ss4)
   ;; (send pb set-before ss5 #f)

   (for ([x (send pb get-snip-stream)])
     (printf "snip: ~a\n" (send x get-key)))

   
   (send frame show #t)
   (sleep/yield 1)))
