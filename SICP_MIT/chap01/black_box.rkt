#lang racket
; 块结构
(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define (sqrt x)
    (define (improve guess)
        (average guess (/ x guess)))
    (define good-enough?
        (lambda (guess)
            (< (abs (- (* guess guess) x)) 0.0001)))
    (define try
        (lambda (guess)
            (if (good-enough? guess)
                guess
                (try (improve guess)))))
    (try 1.0))

(sqrt 100)