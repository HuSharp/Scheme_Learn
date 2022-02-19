#lang racket
; 3-bigger-and-smaller.rkt]
(provide (all-defined-out))
(define bigger 
    (lambda (x y) 
        (if (> x y)
            x
            y)))

(define smaller
    (lambda (x y) 
        (if (< x y)
            x
            y)))