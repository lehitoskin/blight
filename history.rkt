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

#| ############ BEGIN DATABASE STUFF ################ |#
; DATABASE DATABASE! JUST LIVING IN THE DATABASE!
; WOWOW
(define sqlc
  (sqlite3-connect
   #:database db-file
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
;(define messages (get-history "BE69A0EF529FD5248C8CDA83F8545873D9D6734BE3326D49E45610E45A91D427C8A7BA6A433A" "802D30E27746AE299FC2796D014C24700140574BFBFBB9397114D7CB82DC2572"))
(define get-history
  (λ (user contact)
    (let* ([result
           (query sqlc
                  "SELECT * FROM History WHERE userHash = $1 AND contactHash = $2;"
                  user contact)]
           [trimmed (vector-ref (struct->vector result) 2)])
      (map (λ (x) (vector-ref x 3)) trimmed))))
