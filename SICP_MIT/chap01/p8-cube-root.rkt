#lang racket
(define cube
    (lambda (x)
        (* x x x)))

(define square
    (lambda (x) 
        (* x x)))

(define improve
    (lambda (guess x) 
        (/ 
            (+ (/ x (square guess))
                (* 2 guess))
            3)))

(define good-enough?
    (lambda (guess x) 
        (< (abs (- (cube guess) x))
            0.001)))

(define cube-root-iter
    (lambda (guess x) 
        (cond 
            ((good-enough? guess x) guess)
            (else (cube-root-iter (improve guess x)
                    x) ))))

(define cube-root
    (lambda (x) 
        (cube-root-iter 1.0 x)))

(cube-root 1000)