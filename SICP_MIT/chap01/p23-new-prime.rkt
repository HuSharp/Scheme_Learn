#lang racket
(provide (all-defined-out))
(require "prime.rkt")
(require "sum-of-squares.rkt")
(define next
    (lambda (n) 
        (if (= n 2)
            3
            (+ n 2))))

(define find-divisor 
    (lambda (n test-divisor) 
        (cond 
            ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)) ))))

