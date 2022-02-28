#lang racket
(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define average-dump-lambda
    (lambda (f) 
        (lambda (x) 
            (average (f x) x))))

; 以下写法与 average-dump-lambda 等价
(define (average-dump f)
    (define (foo x)
        (average (f x) x))
    foo)