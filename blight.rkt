#!/usr/bin/env racket
#lang racket
; blight
; ncurses-based Tox client - for now
; GUI is better, but I'm the derps with GUI
(require libtoxcore-racket)

(define my-name "Blight Wizard")
(define my-status-message "Toxing on Blight")
(define my-tox (tox_new TOX_ENABLE_IPV6_DEFAULT))

(tox_set_status_message my-tox my-status-message (string-length my-status-message))

(tox_kill my-tox)
