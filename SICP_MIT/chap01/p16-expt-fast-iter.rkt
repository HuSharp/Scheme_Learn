#lang racket
(provide (all-defined-out))
(require "sum-of-squares.rkt")
(require racket/trace)
(define expt-fast
    (lambda (b n) 
        (expt-fast-iter b n 1)))
;think (b^(n/2))^2=(b^2)^(n/2) 
(define expt-fast-iter
    (lambda (b n product) 
        (cond 
            ((= 0 n) product)
            ((even? n) (expt-fast-iter (square b) (/ n 2) product))
            (else (expt-fast-iter b (- n 1) (* product b)) ))))

(trace expt-fast-iter)
(expt-fast 2 15)