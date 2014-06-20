#lang racket/base
; dns.rkt
; implements querying DNS for tox usernames
(require ;ffi/unsafe
  "dns.rkt"
  "ip.rkt"
  ;libtoxcore-racket/dns
  racket/list)

(provide ;tox-dns3
 tox-dns1)

; obtain our nameserver
(define nameserver (make-ip-address (dns-find-nameserver)))

; public key of toxme dns
(define toxme-public-key "")

(define tox-dns1
  (λ (nick domain)
    (cond [(string=? domain "toxme.se")
           (define dns-str (string-append nick "._tox.toxme.se"))
           (let-values ([(auth? qds ans nss ars reply)
                         (dns-query nameserver dns-str 'txt 'in)])
             (define answer (bytes->string/utf-8 (subbytes
                                                  (list->bytes (fifth (first ans)))
                                                  11
                                                  87)))
             answer)]
          [(string=? domain "utox.org")
           (define dns-str (string-append nick "._tox.utox.org"))
           (let-values ([(auth? qds ans nss ars reply)
                         (dns-query nameserver dns-str 'txt 'in)])
             (define answer (bytes->string/utf-8 (subbytes
                                                  (list->bytes (fifth (first ans)))
                                                  11
                                                  87)))
             answer)])))

; public key of utox dns
#|(define utox-public-key
  "D3154F65D28A5B41A05D4AC7E4B39C6B1C233CC857FB365C56E8392737462A12")

; create new dns instance
(define my-dns (tox_dns3_new utox-public-key))
(define dns-ptr (malloc 'atomic (* 1024 (ctype-sizeof _uint8_t))))
(define request-id (malloc 'atomic (ctype-sizeof _uint32_t)))

; procedure to send a tox dns3 request and obtain a
; tox ID from 
(define tox-dns3
  (λ (dns nick domain)
    (cond [(string=? domain "utox.org")
           ; set dns3 string and obtain its length
           (define len
             (tox_generate_dns3_string dns
                                       ; string + 1
                                       (ptr-add dns-ptr 1 _uint8_t)
                                       1023
                                       request-id nick (string-length nick)))
           ; set the first character to "_"
           (ptr-set! dns-ptr _uint8_t (char->integer #\_))
           ; turn the ptr into a byte string
           #;(define byte-str (bytes-append
                             (make-sized-byte-string dns-ptr (- len 8))
                             #"._tox.utox.org"))
           (define byte-str #"_4haaaaipr1o3mz0bxweox541airydbovqlbju51mb4p0ebxq.rlqdj4kkisbep2ks3fj2nvtmk4daduqiueabmexqva1jc._tox.utox.org")
           (let-values ([(auth? qds ans nss ars reply)
                         (dns-query nameserver (bytes->string/utf-8 byte-str) 'txt 'in)])
             (printf "~a\n\n~a\n\n~a\n\n~a\n\n~a\n\n~a\n\n\n" auth? qds ans nss ars reply)
             (define answer (list->bytes (fifth (first ans))))
             (displayln answer))]
          [(string=? domain "toxme.se")
           ; create new dns instance
           (define my-dns (tox_dns3_new toxme-public-key))
           ; set dns3 string and obtain its length
           (define len
             (tox_generate_dns3_string my-dns
                                       ; string + 1
                                       (ptr-add dns-ptr 1 _uint8_t)
                                       1023
                                       request-id nick (string-length nick)))
           ; set the first character to "_"
           (ptr-set! dns-ptr _uint8_t (char->integer #\_))
           ; turn the ptr into a byte string
           (define byte-str (bytes-append
                             (make-sized-byte-string dns-ptr len)
                             #"._tox.toxme.se"))
           ; tox1
           (define dns-str (string-append nick "._tox.toxme.se"))
           (let-values ([(auth? qds ans nss ars reply)
                         (dns-query nameserver dns-str 'txt 'in)])
             (define answer (bytes->string/utf-8 (subbytes
                                                  (list->bytes (fifth (first ans)))
                                                  11
                                                  87)))
             answer)])))

;(tox-dns3 my-dns "leah_twoskin" "toxme.se")
;(tox-dns3 my-dns "leahtwoskin" "utox.org")

(tox_dns3_kill my-dns)|#
