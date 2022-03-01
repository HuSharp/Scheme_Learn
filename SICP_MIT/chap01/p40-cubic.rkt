#lang racket
(require "newton.rkt")

(define (cubic a b c)
    (lambda (x) 
        (+ (* x x x)
            (* a x x)
            (* b x)
            c)))
(display "-------------- cubic -------------\n")
(newton (cubic 3 2 1) 1.0)
(newton (cubic 2 5 5) 1.0)
