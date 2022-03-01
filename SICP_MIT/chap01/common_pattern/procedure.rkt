#lang racket
(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define average-damp-lambda
    (lambda (f) 
        (lambda (x) 
            (average (f x) x))))

; 以下写法与 average-damp-lambda 等价
(define (average-damp f)
    (define (foo x)
        (average (f x) x))
    foo)