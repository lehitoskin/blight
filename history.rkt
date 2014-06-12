#lang racket/base
; history.rkt
; access the sqlite3 database to add to our chat history
(require db/base
         db/sqlite3
         "config.rkt")
(provide sqlc
         add-history
         disconnect)

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
    (query-exec
     sqlc
     (format "INSERT INTO History ~a VALUES (~s,~s,~s,~a,~a);"
             "(userHash, contactHash, message, timestamp, issent)"
             user contact message (current-seconds) sent))))

; get history
; maybe this will be useful for something like
; File -> Chat history -> Select user
; or even Right click user -> View Chat History
;"SELECT * FROM History WHERE userHash = $USER AND contactHash = $CONTACT
;AND timestamp > $OLDEST;"
