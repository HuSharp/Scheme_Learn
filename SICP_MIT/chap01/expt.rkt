#lang racket
(require racket/trace)
; rec
(define expt
    (lambda (b n) 
        (cond 
            ((= n 0) 1)
            (else (* b
                    (expt b (- n 1))) ))))
(trace expt)
(expt 2 5)

(define expt2
    (lambda (b n) 
        (expt-iter b n 1)))

; iter
(define expt-iter
    (lambda (b i product) 
        (cond 
            ((= i 0) product)
            (else (expt-iter b (- i 1) 
                        (* product b))))))
(trace expt-iter)
(expt2 2 5)

(require "sum-of-squares.rkt")
; 也可采用类似 b^2=b*b b^4=b^2*^2
(define expt-fast
    (lambda (b n) 
        (cond 
            ((= 0 n) 1)
            ((even? n) (square (expt-fast b (/ n 2))))
            (else (* b (expt-fast b (- n 1))) ))))

(trace expt-fast)
(expt-fast 2 15)

