#lang racket
; toxdns-test.rkt
(require rackunit
         "../toxdns.rkt"
         "../helpers.rkt")

(define groupbot-id
  "56A1ADE4B65B86BCD51CC73E2CD4E542179F47959FE3E0E21B4B0ACDADE51855D34D34D37CB5")

(define groupbot-id-dns1 (tox-dns1 "groupbot" "toxme.se"))
;(define groupbot-id-dns3 (tox-dns3 "groupbot" "toxme.se"))

(check-true (tox-id? groupbot-id-dns1))
;(check-true? (tox-id? groupbot-id-dns3))

(check-equal? groupbot-id groupbot-id-dns1)
; doesn't work currently because of wrapper problems
;(check-equal? groupbot-id (tox-dns3 "groupbot" "toxme.se"))
