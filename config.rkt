#lang racket/base
; config.rkt
; contains default values for variables
(require json
         racket/bool
         srfi/13
         racket/cmdline
         racket/list
         racket/path)
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
(define profiles
  (λ ([x null])
    ; populate profiles list
    (let* ([ext ".tox"]
           [dlst (directory-list tox-path)]
           [checker (λ (f)
                      (let ([name (path->string f)])
                        (cond [(false? (string-contains-ci name ext)) #f]
                              [else f])))]
           [filtered (filter checker dlst)])
      (make-parameter (map
                       (λ (x)
                         (let [(name (path->string x))]
                           (substring name 0 (- (string-length name) 4))))
                       filtered)))))

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
                 (string-append profile ".tox")))))
; blight-specific configurations
(define config-file
  (λ ([profile (profile-name)])
    (make-parameter
     (build-path tox-path
                 (string-append profile ".json")))))

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
               (build-path icon-dir "available.png")
               (build-path icon-dir "speaker-unmuted.png")
               (build-path icon-dir "speaker-muted.png")))

; max avatar size
(define BLIGHT-MAX-AVATAR-SIZE (expt 2 16))

(define avatar-dir (build-path tox-path "avatars"))

(define logo-dir (build-path "img"))

(define logo (build-path logo-dir "blight-logo-128px.png"))

; tox-path doesn't exist? create it
(unless (directory-exists? tox-path)
  (make-directory tox-path))

(unless (directory-exists? avatar-dir)
  (make-directory avatar-dir))

#|
command-line stuff

--profile: determine the profile to use
--list: list available profiles (by looking for .tox files)
arg: list of files to copy to tox-path as .tox files

1. default profile is "blight"
2. all other files are determined by adding to profile-name
   ex: (string-append (profile-name) ".json")
       (string-append (profile-name) ".tox")
       (string-append (profile-name) ".sqlite")
3. file parameters are curried, which means (db-file) returns a procedure
   and to change it, you must run ((db-file <profile>)), but that is
   only temporary as calling ((db-file)) again will change it back to
   whatever (profile-name) is currently
|#
(let ([ext ".tox"])
  (command-line
   #:usage-help
   "Calling Blight without any arguments will start Blight with the defualt profile."
   "Otherwise, please provide a valid profile for Blight to use."
   "Giving Blight an optional number of files will have them be imported"
   "as Tox profiles."
   #:once-any
   [("-p" "--profile")
    pn ; takes one argument pn
    "Specify the profile (by name) to use at startup. (Do not include a .tox extension.)"
    "Use --list to see a list of available profiles."
    ; given profile has no extension
    (cond [(integer? (string-contains-ci pn ext))
           (displayln "Invalid profile entered! Reverting to default profile...")]
          ; given profile is valid (if it doesn't exist, we'll just make it)
          [else (profile-name pn)
                ((data-file))
                ((config-file))
                ((db-file))])]
   [("-l" "--list") "List available Tox profiles to load."
                    (for-each (λ (f) (displayln f)) ((profiles)))
                    (exit)]
   #:args import-files
   (unless (empty? import-files)
     (for-each
      (λ (x)
        (let* ([fn (path->string (file-name-from-path x))]
               [contains (string-contains-ci fn ext)]
               [timestamp
                (inexact->exact
                 (floor (current-inexact-milliseconds)))])
          (if (false? contains)
              (copy-file x (build-path tox-path
                                       (string-append fn timestamp ext)))
              (copy-file x (build-path tox-path
                                       (substring fn 0 contains))))))
      import-files))))
#| ###################### END COMMAND-LINE STUFF ######################### |#

; if <profile>.json does not exist, create it
(unless (file-exists? ((config-file)))
  (define config-port-out
    (open-output-file ((config-file))
                      #:mode 'text
                      #:exists 'can-update))
  (printf "~a created...\n" ((config-file)))
  (close-output-port config-port-out))

; if <profile>.tox does not exist, create it
(unless (file-exists? ((data-file)))
  (define data-port-out
           (open-output-file ((data-file))
                             #:mode 'binary
                             #:exists 'can-update))
         (printf "~a created...~n" ((data-file)))
         (close-output-port data-port-out))

; default name and status message
; if data exists, do no use these
(define my-name-default "Blight Tester")
(define my-name (make-parameter my-name-default))
(define my-status-message-default "Toxing on Blight")
(define my-status-message (make-parameter my-status-message-default))

; DHT stuff
(struct dht-node (nick address port public-key) #:transparent)
; list of structs containing DHT node information obtained from
; http://wiki.tox.im - change this to be taken from a JSON query
(define node-list
  (list
   (dht-node 'bunslow "76.191.23.96" 33445
             "93574A3FAB7D612FEA29FD8D67D3DD10DFD07A075A5D62E8AF3DD9F5D0932E11")
   (dht-node 'stal "23.226.230.47" 33445
             "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074")
   (dht-node 'nurupo "192.210.149.121" 33445
             "F404ABAA1C99A9D37D61AB54898F56793E1DEF8BD46B1038B9D822E8460FAB67")
   (dht-node 'jfreegman "104.219.184.206" 443
             "8CD087E31C67568103E8C2A28653337E90E6B8EDA0D765D57C6B5172B4F1F04C")))

; if <profile>.json exists, do not use these
(define use-ipv6?-default #t)
(define use-udp?-default #t)
(define proxy-type-default 'none) ; _TOX-PROXY-TYPE value
(define proxy-host-default "") ; ignored if proxy type is 'NONE
(define proxy-port-default 0) ; ignored if proxy type is 'NONE
(define start-port-default 0)
(define end-port-default 0)
(define tcp-port-default 0)
(define encrypted?-default #f)

; if blight-config.json does not exist, initalize it to default values
(define json-default
  (hasheq 'make-noise make-noise-default
          'ipv6? use-ipv6?-default
          'udp? use-udp?-default
          ; turn proxy-type-default into a string because of (jsexpr?)
          'proxy-type (symbol->string proxy-type-default)
          'proxy-host proxy-host-default
          'proxy-port proxy-port-default
          'start-port start-port-default
          'end-port end-port-default
          'tcp-port tcp-port-default
          'encrypted? encrypted?-default))

; <profile>.json is empty, initialize with default values for variables
(when (zero? (file-size ((config-file))))
  (let ([config-port-out (open-output-file ((config-file))
                                           #:mode 'text
                                           #:exists 'truncate/replace)])
    (json-null 'null)
    (write-json json-default config-port-out)
    (write-json (json-null) config-port-out)
    (close-output-port config-port-out)))

(define-syntax-rule (hash-ref* mhash k1 k2 ...)
  (let ([to-set (λ () #f)]) ; in case the key isn't in the hash
    (values (make-parameter (hash-ref mhash k1 to-set))
            (make-parameter (hash-ref mhash k2 to-set)) ...)))

(define-values
  (make-noise use-ipv6? use-udp? proxy-type proxy-host
              proxy-port start-port end-port tcp-port encrypted?)
  (let* ([config-port-in (open-input-file ((config-file)) #:mode 'text)]
         [json-info (read-json config-port-in)])
    (close-input-port config-port-in)
    (hash-ref* json-info 'make-noise 'ipv6? 'udp? 'proxy-type 'proxy-host
               'proxy-port 'start-port 'end-port 'tcp-port 'encrypted?)))

; _TOX-PROXY-TYPE is a symbol
(proxy-type (string->symbol (proxy-type)))

(define (toggle-noise) (make-noise (not (make-noise))))

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
