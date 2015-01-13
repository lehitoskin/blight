#!/usr/bin/env racket
#lang racket/base
; main.rkt
; the file that contains the main tox loops
(require racket/class
         "tox.rkt"
         "gui/frame.rkt"
         "gui/menu-bar.rkt"
         "gui/friend-list.rkt"
         "gui/preferences.rkt"
         "gui/profiles.rkt"
         "gui/add-friend.rkt"
         "gui/add-group.rkt"
         "blight.rkt"
         "callbacks.rkt"
         "chat.rkt"
         "config.rkt"
         "group.rkt"
         "helpers.rkt"
         "msg-editor.rkt"
         "msg-history.rkt"
         "repl.rkt"
         "smart-list.rkt"
         "toxdns.rkt"
         "utils.rkt")

(send frame show #t)
(init-repl)
