#lang racket
(define fib
    (lambda (x) 
        (cond 
            ((< x 2) x)
            (else (+ (fib (- x 1)) (fib (- x 2)))))))
            
(fib 5)
(fib 6)
(fib 7)

(define fib2
    (lambda (n) 
        (fib-iter 0 1 n)))

(define fib-iter
    (lambda (a b cnt) 
        (cond 
            ((= 0 cnt) a)
            (else (fib-iter b (+ a b) (- cnt 1)) ))))

(fib2 5)
(fib2 6)
(fib2 7)