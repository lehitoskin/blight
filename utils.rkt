#lang racket

(provide (all-defined-out))

(define (repeat proc times)
  (cond [(zero? times) #t]
        [else (proc) (repeat proc (- times 1))]))
