#!/usr/bin/env racket
#lang racket/base
; main.rkt
; the file that contains the main tox loops
(require racket/class
         "tox.rkt"
         "gui/avatar.rkt"
         "gui/frame.rkt"
         "gui/menu-bar.rkt"
         "gui/friend-list.rkt"
         "gui/preferences.rkt"
         "gui/profiles.rkt"
         "gui/add-friend.rkt"
         "gui/add-group.rkt"
         "gui/msg-editor.rkt"
         "gui/msg-history.rkt"
         "gui/chat.rkt"
         "gui/group.rkt"
         "gui/smart-list.rkt"
         "blight.rkt"
         "callbacks.rkt"
         "config.rkt"
         "helpers.rkt"
         "dns/toxdns.rkt"
         "utils.rkt")

(send frame show #t)
(init-repl)
