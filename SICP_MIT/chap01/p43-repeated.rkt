#lang racket
(provide (all-defined-out))
(require "p42-compose.rkt")

(define (repeated f n)
    (cond 
        ((= n 0) 1)
        ((= n 1) f)
        (else (compose (repeated f (- n 1)) f) )))

; iter
(define (repeated-iter f n)
    (define (iter i repeated-f)
        (cond 
            ((= i 0) 1)
            ((= i 1) repeated-f)
            (else (iter (- i 1)
                        (compose f repeated-f)))))
    (iter n f))

(define (square x)
    (* x x))
((repeated square 2) 5)
((repeated-iter square 2) 5)