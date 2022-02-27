#lang racket
(require "p37-cont-frac.rkt")
(define (tan-cf x k)
    (define (N i)
        (if (= i 1)
            x
            (- (* x x))))
    (define (D i) (- (* i 2) 1))
            
    (exact->inexact (cont-frac N D k)))

(tan 10)
(tan-cf 10 100)