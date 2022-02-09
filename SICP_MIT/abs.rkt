#lang racket
(define (abs x)
    (cond 
        ((zero? x) 0)
        ((> x 0) x)
        (else (- x))))

(abs 100)
(abs -100)
(abs 0)

(define (abs2 x)
    (if (< x 0)
        (- x)
        x))

(abs2 100)
(abs2 -100)
(abs2 0)