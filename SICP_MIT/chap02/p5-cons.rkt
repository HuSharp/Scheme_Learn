#lang racket
(define (cons-multi x y)
    (* (expt 2 x)
        (expt 3 y)))

(cons-multi 3 2)

; 主要思路是：
; 对 2 一直除， 取除的次数
(define (car-multi x)
    (if (= 0 (remainder x 2))
        (+ 1 (car-multi (/ x 2)))
        0))

(define (cdr-multi x)
    (if (= 0 (remainder x 3))
        (+ 1 (cdr-multi (/ x 3)))
        0))

(car-multi (cons-multi 3 2))
(cdr-multi (cons-multi 3 2))