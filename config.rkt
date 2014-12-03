#lang racket/base
; config.rkt
; contains default values for variables
(require json)
(provide (all-defined-out))

; base tox directory
(define tox-path (cond [(eq? (system-type) 'unix)
                        (build-path (find-system-path 'home-dir)
                                    ".config/tox")]
                       [(eq? (system-type) 'windows)
                        (normal-case-path
                         (build-path (find-system-path 'home-dir)
                                     "appdata/local/tox"))]
                       [(eq? (system-type) 'macosx)
                        (build-path (find-system-path 'home-dir)
                                    "Library/Application Support/tox")]))

(define download-path (normal-case-path (build-path (find-system-path 'home-dir)
                                                    "Downloads")))

; profiling!
(define default-profile "blight")
(define profile-name (make-parameter default-profile))

; history db file
(define db-file
  (λ ([profile (profile-name)])
    (make-parameter
     (build-path tox-path
                 (string-append profile ".sqlite")))))
; tox-specific information
(define data-file
  (λ ([profile (profile-name)])
    (make-parameter
     (build-path tox-path
                 (string-append (profile-name) ".tox")))))
; blight-specific configurations
(define config-file
  (λ ([profile (profile-name)])
    (make-parameter
     (build-path tox-path
                 (string-append (profile-name) ".json")))))

; location of sound directory (currently depends on running from same dir
; change to /usr/share/blight/sounds (or something) once a proper
; installer is to be had
(define sound-dir (build-path "sounds"))
; list of sound files
(define sounds (list
                (build-path sound-dir "New Message.wav")
                (build-path sound-dir "Contact Logs In.wav")
                (build-path sound-dir "Contact Logs Out.wav")
                (build-path sound-dir "Log In.wav")
                (build-path sound-dir "Log Out.wav")
                (build-path sound-dir "Contact Request Accepted.wav")
                (build-path sound-dir "Transfer Pending.wav")
                (build-path sound-dir "Transfer Complete.wav")
                (build-path sound-dir "Incoming Call.wav")
                (build-path sound-dir "Outgoing Call.wav")
                (build-path sound-dir "Incoming Video Call.wav")
                (build-path sound-dir "Outgoing Video Call.wav")
                ; error sound will be the last element
                (build-path sound-dir "Error.wav")))

(define make-noise-default #t)

; blight icons for the buddy list
(define icon-dir (build-path "icons"))
; list of icon files
(define icons (list
               (build-path icon-dir "offline.png")
               (build-path icon-dir "busy.png")
               (build-path icon-dir "away.png")
               (build-path icon-dir "groupchat.png")
               (build-path icon-dir "available.png")))

(define avatar-dir (build-path tox-path "avatars"))

; tox-path doesn't exist? create it
(unless (directory-exists? tox-path)
  (make-directory tox-path))

(unless (directory-exists? avatar-dir)
  (make-directory avatar-dir))

; if blight-config.json does not exist, create it
(unless (file-exists? ((config-file)))
  (define config-port-out
    (open-output-file config-file
                      #:mode 'text
                      #:exists 'can-update))
  (printf "~a created...\n" ((config-file)))
  (close-output-port config-port-out))

; open blight-config.json
(define config-port-in (open-input-file ((config-file))
                                        #:mode 'text))

(let ([old-data-file (build-path tox-path "blight-data")])
  ; if old data-file exists, but new one does not,
  ; copy it over, otherwise do nothing
  (cond [(and (not (file-exists? ((data-file))))
              (file-exists? old-data-file))
         (printf "Detected old data file ~a, copying to ~a... " old-data-file ((data-file)))
         (copy-file old-data-file ((data-file)))
         (displayln "Done!")]
        ; if neither exist, this must all be new, so create a fresh data-file
        [(and (not (file-exists? ((data-file))))
              (not (file-exists? old-data-file)))
         (define data-port-out
           (open-output-file ((data-file))
                             #:mode 'binary
                             #:exists 'can-update))
         (printf "~a created...~n" ((data-file)))
         (close-output-port data-port-out)]))

; default name and status message
; if data exists, do no use these
(define my-name-default "Blight Tester")
(define my-status-message-default "Toxing on Blight")

; default DHT settings
; if blight-config.json exists, do not use these
(define dht-address-default "23.226.230.47")
(define dht-port-default 33445)
(define dht-public-key-default
  "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074")

; if blight-config.json does not exist, initalize it to default values
(define json-default
  (hasheq 'dht-address dht-address-default
          'dht-port dht-port-default
          'dht-public-key dht-public-key-default
          'my-name-last my-name-default
          'my-status-last my-status-message-default
          'make-noise-last make-noise-default))

; blight-config.json is empty, initialize with default values for variables
(unless (not (zero? (file-size ((config-file)))))
  (let ([config-port-out (open-output-file ((config-file))
                                           #:mode 'text
                                           #:exists 'truncate/replace)])
    (json-null 'null)
    (write-json json-default config-port-out)
    (write-json (json-null) config-port-out)
    (close-output-port config-port-out)))

; read from blight-config.json
(define json-info (read-json config-port-in))
; set variables to values those contained in blight-config.json
(define dht-address (hash-ref json-info 'dht-address))
(define dht-port (hash-ref json-info 'dht-port))
(define dht-public-key (hash-ref json-info 'dht-public-key))
(define my-name (hash-ref json-info 'my-name-last))
(define my-status-message (hash-ref json-info 'my-status-last))
(define make-noise (hash-ref json-info 'make-noise-last))
(define toggle-noise (λ () (set! make-noise (not make-noise))))

; list of unicode emoticons
(define emojis (list "😁" "😂" "😃" "😄" "😅" "😇"
                     "😈" "😉" "😊" "😋" "😌" "😍"
                     "😎" "😏" "😐" "😒" "😓" "😔"
                     "😖" "😘" "😚" "😜" "😝" "😞"
                     "😠" "😡" "😢" "😣" "😥" "😨"
                     "😩" "😪" "😫" "😭" "😰" "😱"
                     "😲" "😳" "😵" "😶" "😷" "😸"
                     "😹" "😺" "😻" "😼" "😽" "😾"
                     "😿" "🙀" "☺" "☹" "⚇" "🐱"
                     "♥" "☔" "☀" "♫" "☕" "★"))
