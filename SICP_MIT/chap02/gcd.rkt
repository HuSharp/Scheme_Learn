#lang racket
(provide (all-defined-out))
(define gcd
    (lambda (a b) 
        (cond 
            ((= b 0) a)
            (else (gcd b (remainder a b)) ))))

(gcd 206 40)        ; 2
(gcd 204 40)        ; 4