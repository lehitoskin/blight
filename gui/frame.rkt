#lang racket/gui
; gui.rkt
; contains a lot of the gui definitions
(require libtoxcore-racket/functions
         libtoxcore-racket/enums
         (only-in pict
                  bitmap
                  pict->bitmap
                  scale-to-fit)
         "../config.rkt"
         "../tox.rkt"
         "avatar.rkt")
(provide (all-defined-out))

(define license-message
  "Blight - a Tox client written in Racket.
Copyright (C) 2014 Lehi Toskin.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.


Tox's sounds are licensed under the \"Creative Commons Attribution 3.0
Unported\", all credit attributed to Adam Reid.")

(define get-help-message
  "Need more help? Try adding leahtwoskin@toxme.se (or leahtwoskin@utox.org)
and bug the dev! Alternatively, you could join #tox-dev on freenode and see
if people have a similar problem.")

; chat entity holding group or contact data
(define cur-groups (make-hash))
(define cur-buddies (make-hash))

#| #################### BEGIN GUI STUFF ######################## |#
; create a new top-level window
; make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "Blight - Friend List"]
                   [stretchable-width #t]
                   [height 600]))

; set the frame icon
(let ([icon-bmp (make-bitmap 32 32)])
  (send icon-bmp load-file logo)
  (send frame set-icon icon-bmp))

; make a static text message in the frame
(define frame-msg (new message%
                       [parent frame]
                       [label "Blight Friend List"]))

(define frame-hpanel (new horizontal-panel%
                          [parent frame]
                          [alignment '(left center)]))

(define frame-avatar-button
  (new button%
       [parent frame-hpanel]
       [label (my-avatar)]
       [callback
        (λ (button event)
          (thread
           (λ ()
             (let ([path (get-file "Select an avatar" ; message
                                   #f ; parent
                                   #f ; directory
                                   #f ; filename
                                   "png" ; extension (windows only)
                                   null ; style
                                   '(("PNG" "*.png")))]) ; filters
               (unless (false? path)
                 (let* ([img-data (file->bytes path)]
                        [my-client-id (substring (my-id-hex) 0 (* TOX_CLIENT_ID_SIZE 2))]
                        [avatar-file (build-path avatar-dir
                                                 (string-append my-client-id ".png"))]
                        [hash-file (build-path avatar-dir
                                               (string-append my-client-id ".hash"))])
                   (displayln "Setting avatar...")
                   ; create a temp bitmap
                   (define avatar-bitmap (make-bitmap 40 40))
                   ; load the file in to the bitmap
                   (send avatar-bitmap load-file path)
                   ; turn it into a pict
                   (define avatar-pict (bitmap avatar-bitmap))
                   ; scale the pict to 40x40
                   (define avatar-pict-small (scale-to-fit avatar-pict 40 40))
                   ; set the avatar in tox
                   (set-avatar! my-tox (_TOX_AVATAR_FORMAT 'PNG) img-data)
                   ; set the avatar to the new one
                   (my-avatar (pict->bitmap avatar-pict-small))
                   ; save the avatar to avatar directory
                   (copy-file path avatar-file #t)
                   ; save the hash to the same dir
                   (let ([hash-port-out (open-output-file hash-file
                                                        #:mode 'binary
                                                        #:exists 'truncate/replace)])
                     (define my-hash (tox-hash img-data))
                     (write-bytes my-hash hash-port-out)
                     (close-output-port hash-port-out))
                   ; reset the avatar as this button's label
                   (send button set-label (my-avatar))
                   ; broadcast to our friends we've changed our avatar
                   (displayln "Broadcasting our avatar change to online friends...")
                   (for ([count (hash-count cur-buddies)])
                     (when (= 1 (get-friend-connection-status my-tox count))
                       (send-avatar-info my-tox count)))))))))]))

(define frame-vpanel (new vertical-panel%
                          [parent frame-hpanel]
                          [alignment '(left center)]))

(define username-frame-message (new message%
                                    [parent frame-vpanel]
                                    [label my-name]))

(send username-frame-message auto-resize #t)

(define status-frame-message (new message%
                                  [parent frame-vpanel]
                                  [label my-status-message]))

(send status-frame-message auto-resize #t)

; choices for status type changes
(define status-choice
  (new choice%
       [parent frame]
       [label #f]
       [stretchable-width #t]
       [choices '("Available"
                  "Away"
                  "Busy")]
       [selection (get-self-user-status my-tox)]
       [callback (λ (choice control-event)
                   (set-user-status! my-tox (send choice get-selection)))]))
