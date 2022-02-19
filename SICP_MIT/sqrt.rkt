#lang racket
(define good-enough?
    (lambda (guess x)
        (< (abs (- (* guess guess) (* x x))) 0.0001)))

(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define improve
    (lambda (guess x) 
        (average guess (/ x guess))))

(define try
    (lambda (guess x)
        (if (good-enough? guess x)
            guess
            (try (improve guess x) x))))

(define sqrt 
    (lambda (x) 
        (try 1 x)))

(sqrt 100)