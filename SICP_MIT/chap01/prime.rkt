#lang racket
(provide (all-defined-out))
(require "sum-of-squares.rkt")
(define smallest-divisor
    (lambda (n) 
        (find-divisor n 2)))

(define find-divisor
    (lambda (n test-divisor) 
        (cond 
            ((> (square test-divisor) n) n)         ; 
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)) ))))

(define divides?
    (lambda (a b) 
        (= (remainder b a) 0)))

; n is prime just when smallest-divisor is itself
(define prime?
    (lambda (n) 
        (= n (smallest-divisor n))))

(prime? 100)         ; f
(prime? 97)          ; t

(smallest-divisor 199)      ; 199
(smallest-divisor 1999)     ; 1999
(smallest-divisor 19999)    ; 7