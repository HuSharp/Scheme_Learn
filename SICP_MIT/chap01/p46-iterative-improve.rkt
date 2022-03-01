#lang racket
(define (iterative-improve close-enough? improve)
    (lambda (first-guess) 
        (define (try guess)
            (let ((next (improve guess)))
                (if (close-enough? guess next)
                    next
                    (try next))))
    (try first-guess)))

(define tolerance 0.00001)
(define (close-enough? f1 f2)
        (< (abs (- f1 f2)) tolerance))

(define (fixed-point f first-guess)
    (define (improve guess)
        (f guess))
    ((iterative-improve close-enough? improve) first-guess))
(fixed-point cos 1.0)


(define (sqrt x)
    (define (improve guess)
        (average guess (/ x guess)))
    ((iterative-improve close-enough? improve) 1.0))

(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(sqrt 9)