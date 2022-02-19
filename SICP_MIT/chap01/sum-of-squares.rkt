#lang racket
(provide (all-defined-out))
(define square
    (lambda (x) 
        (* x x)))

(define sum-of-squares
    (lambda (x y) 
        (+ (square x)
            (square y))))