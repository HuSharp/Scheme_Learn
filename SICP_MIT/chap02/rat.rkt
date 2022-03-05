#lang racket
(require "gcd.rkt")
(define (add-rat x y)
    (make-rat (+ (* (number x) (denom y)) 
                (* (number y) (denom x)))
                (* (denom y) (denom x))))

(define (make-rat n d) 
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
    
(define (number x) (car x))
; denominator
(define (denom x) (cdr x))

(define (print-rat x)
    (display (number x))
    (display "/")
    (display (denom x))
    (newline))

; 1/2 + 1/4
(define A (make-rat 1 2))
(define B (make-rat 1 4))

(print-rat (add-rat A B))