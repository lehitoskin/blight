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
               (build-path icon-dir "available.png")))

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
                    (for-each (λ (f)
                                (let ([name (path->string f)])
                                  (displayln
                                   (substring name 0 (- (string-length name) 4)))))
                              ((profiles)))
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

#| ###################### OLD FILE IMPORTATION ########################## |#
; migrate old config file
(let ([old-config-file (build-path tox-path "blight-config.json")]
      [new-config-file (build-path tox-path "blight.json")])
  (cond [(and (not (file-exists? new-config-file))
              (file-exists? old-config-file))
         (printf "Detected old config file ~a, copying to ~a... "
                 old-config-file new-config-file)
         (copy-file old-config-file new-config-file)
         (displayln "Done!")
         (print "Updating config file... ")
         ; update json hasheq to contain new variable 'profile-last
         (let* ([config-port-in (open-input-file new-config-file
                                                 #:mode 'text)]
                [json-info-old (read-json config-port-in)]
                [json-info-new (hash-set json-info-old
                                         'profile-last default-profile)]
                [config-port-out (open-output-file new-config-file
                                                   #:mode 'text
                                                   #:exists 'truncate/replace)])
           (json-null 'null)
           (write-json json-info-new config-port-out)
           (write-json (json-null) config-port-out)
           (close-input-port config-port-in)
           (close-output-port config-port-out)
           (displayln "Done!"))]))

; migrate old data file
(let ([old-data-file (build-path tox-path "blight-data")]
      [new-data-file (build-path tox-path "blight.tox")])
  ; if old data-file exists, but new one does not,
  ; copy it over, otherwise do nothing
  (cond [(and (not (file-exists? new-data-file))
              (file-exists? old-data-file))
         (printf "Detected old data file ~a, copying to ~a... " old-data-file new-data-file)
         (copy-file old-data-file new-data-file)
         (displayln "Done!")]))

; migrate old history database
(let ([old-db-file (build-path tox-path "blight-tox.db")]
      [new-db-file (build-path tox-path "blight.sqlite")])
  (cond [(and (not (file-exists? new-db-file))
              (file-exists? old-db-file))
         (printf "Detected old history database ~a, copying to ~a... " old-db-file new-db-file)
         (copy-file old-db-file new-db-file)
         (displayln "Done!")]))
 #| ##################### END OLD FILE IMPORTATION ############################ |#

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
(define my-status-message-default "Toxing on Blight")

; default DHT settings
; if <profile>.json exists, do not use these
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
          'make-noise-last make-noise-default
          'profile-last default-profile))

; <profile>.json is empty, initialize with default values for variables
(unless (not (zero? (file-size ((config-file)))))
  (let ([config-port-out (open-output-file ((config-file))
                                           #:mode 'text
                                           #:exists 'truncate/replace)])
    (json-null 'null)
    (write-json json-default config-port-out)
    (write-json (json-null) config-port-out)
    (close-output-port config-port-out)))

; read from <profile>.json
(define json-info (let* ([config-port-in
                          (open-input-file ((config-file))
                                           #:mode 'text)]
                         [my-json (read-json config-port-in)])
                    (close-input-port config-port-in)
                    my-json))
; set variables to values those contained in <profile>.json
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
