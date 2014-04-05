#!/usr/bin/env racket
#lang racket
; blight
; ncurses-based Tox client - for now
; GUI is better, but I'm the derps with GUI
(require libtoxcore-racket)

(define my-name "Blight Wizard")
(define my-user-status "Toxing on Blight")
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))

(tox_set_self_user_status my-user-status (string-length my-user-status))

(tox_kill my-tox)
