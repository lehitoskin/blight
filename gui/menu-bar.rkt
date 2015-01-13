#lang racket/gui
; menu-bar.rkt
; contains definitions for the buddy list's menu bar
(require "frame.rkt"
         "preferences.rkt"
         "profiles.rkt"
         "../blight.rkt"
         "../config.rkt"
         "../chat.rkt"
         "../msg-history.rkt"
         "../tox.rkt")

(provide (all-defined-out))

#| ################### BEGIN MENU BAR STUFF #################### |#
; menu bar for the frame
(define frame-menu-bar (new menu-bar%
                            [parent frame]))

; menu File for menu bar
(define menu-file (new menu%
                       [parent frame-menu-bar]
                       [label "&File"]
                       [help-string "Open, Quit, etc."]))

; Copy ID to Clipboard item for File
(define menu-copy-id
  (new menu-item%
       [parent menu-file]
       [label "Copy My ID to Clipboard"]
       [help-string "Copies your Tox ID to the clipboard"]
       [callback (λ (button event)
                   ; copy id to clipboard
                   (send chat-clipboard set-clipboard-string (my-id-hex) (current-seconds)))]))

; dialog box when exiting
(define exit-dialog (new dialog%
                         [label "Exit Blight"]
                         [style (list 'close-button)]))

; Quit menu item for File
; uses message-box with 'ok-cancel
(define menu-quit
  (new menu-item%
       [parent menu-file]
       [label "&Quit"]
       [shortcut #\Q]
       [help-string "Quit Blight"]
       [callback (λ (button event)
                   (let ([mbox (message-box/custom
                                "Blight - Quit Blight"
                                "Are you sure you want to quit Blight?"
                                "&OK"
                                "&Cancel"
                                #f
                                exit-dialog
                                (list 'caution 'no-default))])
                     (cond [(= mbox 1) (clean-up) (exit)])))]))

; menu Edit for menu bar
(define menu-edit (new menu%
                       [parent frame-menu-bar]
                       [label "&Edit"]
                       [help-string "Modify Blight"]))

; Preferences menu item for Edit
(define menu-preferences (new menu-item%
                              [parent menu-edit]
                              [label "Preferences"]
                              [shortcut #\R]
                              [help-string "Modify Blight preferences"]
                              [callback (λ (button event)
                                          (thread
                                           (λ ()
                                             (send preferences-box show #t))))]))

(define menu-profile (new menu-item%
                          [parent menu-edit]
                          [label "Profiles"]
                          [shortcut #\P]
                          [help-string "Manage Tox profiles"]
                          [callback (λ (button event)
                                      (thread
                                       (λ ()
                                         (send profiles-box show #t))))]))

(define help-get-dialog (new dialog%
                             [label "Blight - Get Help"]
                             [style (list 'close-button)]))

(define help-get-text (new text%
                           [line-spacing 1.0]
                           [auto-wrap #t]))
(send help-get-text change-style black-style)
(send help-get-text insert get-help-message)

(define help-get-editor-canvas
  (new editor-canvas%
       [parent help-get-dialog]
       [min-height 100]
       [min-width 600]
       [vert-margin 10]
       [editor help-get-text]
       [style (list 'control-border 'no-hscroll
                    'auto-vscroll 'no-focus)]))

(define help-get-ok
  (new button%
       [parent help-get-dialog]
       [label "&OK"]
       [callback (λ (button event)
                   (send help-get-dialog show #f))]))

; dialog box when looking at Help -> About
(define help-about-dialog (new dialog%
                               [label "Blight - License"]
                               [style (list 'close-button)]))

(define help-about-text (new text%
                             [line-spacing 1.0]
                             [auto-wrap #t]))
(send help-about-text change-style black-style)
(send help-about-text insert license-message)

; canvas to print the license message
(define help-about-editor-canvas
  (new editor-canvas%
       [parent help-about-dialog]
       [min-height 380]
       [min-width 600]
       [vert-margin 10]
       [editor help-about-text]
       [style (list 'control-border 'no-hscroll
                    'auto-vscroll 'no-focus)]))

; button to close the About Blight window
(define help-about-ok
  (new button%
       [parent help-about-dialog]
       [label "&OK"]
       [callback (λ (button event)
                   (send help-about-dialog show #f))]))

; menu Help for menu bar
(define menu-help (new menu%
                       [parent frame-menu-bar]
                       [label "&Help"]
                       [help-string "Get information about Blight"]))

; About Blight menu item for Help
(define menu-help-get-help (new menu-item%
                                [parent menu-help]
                                [label "Get Help"]
                                [help-string "Get Help with Blight"]
                                [callback (λ (button event)
                                            (send help-get-dialog show #t))]))

; About Blight menu item for Help
(define menu-help-about (new menu-item%
                             [parent menu-help]
                             [label "About Blight"]
                             [help-string "Show information about Blight"]
                             [callback (λ (button event)
                                         (send help-about-dialog show #t))]))
#| #################### END MENU BAR STUFF ################## |#
