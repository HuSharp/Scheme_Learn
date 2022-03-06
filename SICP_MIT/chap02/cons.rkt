#lang racket
(define (cons2 x y)
    (define (dispatch m)
        (cond 
            ((= m 0) x)
            ((= m 1) y)
            (else (error "Argument not 0 or 1 -- CONS" m))))
    dispatch)

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

(car2 (cons2 2 3))