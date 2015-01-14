#lang racket/base
; dns.rkt
; implements querying DNS for tox usernames
(require "dns.rkt"
         "ip.rkt"
         "../utils.rkt"
         libtoxcore-racket/dns
         libtoxcore-racket/blight
         libtoxcore-racket/functions
         racket/bool
         racket/list)

(provide tox-dns1
         tox-dns3)

; obtain our nameserver
(define nameserver (make-ip-address (dns-find-nameserver)))

; public key of toxme dns
(define toxme-public-key
  "5D72C517DF6AEC54F1E977A6B6F25914EA4CF7277A85027CD9F5196DF17E0B13")
; public key of utox dns
(define utox-public-key
  "D3154F65D28A5B41A05D4AC7E4B39C6B1C233CC857FB365C56E8392737462A12")

; pubkey in bytes to use with the dns3 procedure
(define toxme-pubkey-bytes
  (hex-string->bytes toxme-public-key (/ (string-length toxme-public-key) 2)))
; utox pubkey in bytes
(define utox-pubkey-bytes
  (hex-string->bytes utox-public-key (/ (string-length utox-public-key) 2)))

; srv needs to be a nameserver
; dns-str needs to be a string
; start and stop are for parsing the returned information
(define custom-dns-query
  (λ (srv dns-str start stop)
    (let-values ([(auth? qds ans nss ars reply)
                  (dns-query srv dns-str 'txt 'in)])
      (cond [(or (empty? ans)
                 (= (length (fifth (first ans))) 1)) #f]
            [else (bytes->string/utf-8 (subbytes
                                        (list->bytes (fifth (first ans)))
                                        start stop))]))))

(define tox-dns1
  (λ (nick domain)
    (cond [(string=? domain "toxme.se")
           (define dns-str (string-append nick "._tox.toxme.se"))
           (define response (custom-dns-query nameserver dns-str 11 87))
           (if (false? response)
               #f
               response)]
          [(string=? domain "utox.org")
           (define dns-str (string-append nick "._tox.utox.org"))
           (define response (custom-dns-query nameserver dns-str 11 87))
           (if (false? response)
               #f
               response)])))

; procedure to send a tox dns3 request and obtain a tox ID
; in an encrypted manner
(define tox-dns3
  (λ (nick domain)
    ; we're sending to toxme.se
    (cond [(string=? domain "toxme.se")
           (let* ([dns3 (dns3-new toxme-pubkey-bytes)])
             ; obtain the length of the resultant dns3 string
             (define-values (request-id bstr) (dns3-encrypt dns3 nick))
             ; if bstr is #f then the procedure failed, return #f
             (cond [(false? bstr) #f]
                   [else
                    (define query
                      (bytes->string/utf-8
                       (bytes-append #"_" bstr #"._tox.toxme.se")))
                    (define enc-response
                      (string->bytes/utf-8
                       (custom-dns-query nameserver query 11 98)))
                    (if (false? enc-response)
                        #f
                        (let ([result
                               (dns3-decrypt dns3 enc-response request-id)])
                          (dns3-kill! dns3)
                          (if (false? result)
                              #f
                              (bytes->hex-string result))))]))]
          ; we're sending to utox.org
          [(string=? domain "utox.org")
           (let* ([dns3 (dns3-new utox-pubkey-bytes)])
             ; obtain length of the resultant dns3 string
             (define-values (request-id bstr) (dns3-encrypt dns3 nick))
             (cond [(false? bstr) #f]
                   [else
                    (define query
                      (bytes->string/utf-8
                       (bytes-append #"_" bstr #"._tox.utox.org")))
                    (define enc-response
                      (string->bytes/utf-8
                       (custom-dns-query nameserver query 11 98)))
                    (if (false? enc-response)
                        #f
                        (let ([result (dns3-decrypt dns3
                                                    enc-response
                                                    request-id)])
                          (dns3-kill! dns3)
                          (if (false? result)
                              #f
                              (bytes->hex-string result))))]))])))
