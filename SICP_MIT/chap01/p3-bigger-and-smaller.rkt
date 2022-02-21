#lang racket
(require "sum-of-squares.rkt")
(require "3-bigger-and-smaller.rkt")

(define bigger-sum-of-squares
    (lambda (x y z) 
        (sum-of-squares (bigger x y)
                        (bigger (smaller x y) z))))


(bigger-sum-of-squares 0 2 2)     ; 2^2 + 2^2 = 4 + 4 = 8
;Value: 8

(bigger-sum-of-squares 1 2 3)     ; 2^2 + 3^2 = 4 + 9 = 13
;Value: 13

(bigger-sum-of-squares 3 5 7)     ; 5^2 + 7^2 = 25 + 49 = 74
;Value: 74