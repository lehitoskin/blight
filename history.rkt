#lang racket/base
; history.rkt
; access the sqlite3 database to add to our chat history
(require db/base
         db/sqlite3
         "config.rkt")
(provide sqlc
         add-history
         disconnect
         get-history)

(printf "db-file is currently ~a~n" ((db-file)))
; copy old file scheme to follow new file scheme
(let ([old-db-file (build-path tox-path "blight-tox.db")])
  (cond [(and (not (file-exists? ((db-file))))
              (file-exists? old-db-file))
         (printf "Detected old history database ~a, copying to ~a... " old-db-file ((db-file)))
         (copy-file old-db-file ((db-file)))
         (displayln "Done!")]))

(printf "db-file is currently ~a~n" ((db-file)))
#| ############ BEGIN DATABASE STUFF ################ |#
; DATABASE DATABASE! JUST LIVING IN THE DATABASE!
; WOWOW
(define sqlc
  (sqlite3-connect
   #:database ((db-file))
   #:mode 'create))

; database initialization
; follows Venom's database scheme
(query-exec sqlc
            "create table if not exists History(
             id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
             userHash TEXT NOT NULL,
             contactHash TEXT NOT NULL,
             message TEXT NOT NULL,
             timestamp INTEGER NOT NULL,
             issent INTEGER NOT NULL);")

; index query
(query-exec sqlc
            "CREATE UNIQUE INDEX IF NOT EXISTS main_index
             ON History (userHash, contactHash, timestamp);")

#|
insert into history

"INSERT INTO History (userHash, contactHash, message, timestamp, issent)
VALUES ($USER, $CONTACT, $MESSAGE, $TIME, $SENDER);"

user is our id
contact is our friend's public key
message is the message itself
timestamp is current-seconds
sent is 0: they sent; 1: we sent
|#
(define add-history
  (λ (user contact message sent)
     (let ([timestamp
            (inexact->exact
             (floor (current-inexact-milliseconds)))])

       (with-handlers ([exn?
                        (lambda (ex)
                          (eprintf "Exception in add-history\n. Args: ~a ~a ~a ~a ~a: ~a\n" user contact message timestamp sent ex))])
         (query-exec
          sqlc
          "INSERT INTO History (userHash, contactHash, message, timestamp, issent)
     VALUES ($1,$2,$3,$4,$5);"
          user contact message timestamp sent)))))

; get history
; maybe this will be useful for something like
; File -> Chat history -> Select user
; or even Right click user -> View Chat History
;"SELECT * FROM History WHERE userHash = $USER AND contactHash = $CONTACT
;AND timestamp > $OLDEST;"
(define get-history
  (λ (user contact)
    (let* ([result
           (query sqlc
                  "SELECT * FROM History WHERE userHash = $1 AND contactHash = $2;"
                  user contact)]
           [trimmed (vector-ref (struct->vector result) 2)]
           [message (map (λ (x) (vector-ref x 3)) trimmed)]
           [timestamp (map (λ (x) (vector-ref x 4)) trimmed)]
           [who (map (λ (x) (vector-ref x 5)) trimmed)])
      (values message timestamp who))))
