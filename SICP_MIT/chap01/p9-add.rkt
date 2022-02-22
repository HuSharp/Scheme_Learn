#lang racket
(define add1
    (lambda (x) 
        (+ x 1)))
(define sub1
    (lambda (x) 
        (- x 1)))

; peano arithmetic
(define o+
    (lambda (x y) 
        (cond 
            ((= 0 x) y)
            (else (+ (sub1 x) (add1 y)) ))))

(define o+2
    (lambda (x y) 
        (cond 
            ((= 0 x) y)
            (else (add1 (o+2 (sub1 x) y)) ))))

(o+ 2 5)
(o+2 2 5)
