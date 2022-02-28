#lang racket
(provide (all-defined-out))
; 表示 N D 过程
; 下面写法会导致 1.1.38 和 1.1.39 由于振荡而无法调用
(define (cont-frac N D k)
    (define (cnt i)
        (cond 
            ((= i k) (/ (N k) (D k)))
            (else  (/ (N i) (+ (D i) (cnt (+ i 1)))))))
    (cnt 1))

(define (good-enough? x y)
    (< (abs (- x y)) 0.0001))

(require "p35-golden-ratio.rkt")
(newline)
(good-enough? golden-ratio
    (cont-frac
        (lambda (i) 1.0)
        (lambda (i) 1.0)
        9))
(good-enough? golden-ratio
    (cont-frac
        (lambda (i) 1.0)
        (lambda (i) 1.0)
        10))

(define (cont-frac-iter N D k)
    (define (cnt i result)
        (cond 
            ((= i 0) result)
            (else (cnt (- i 1)
                        (/ (N i) (+ (D i) result))))))
    (cnt k (/ (N k) (D k))))

(good-enough? golden-ratio
    (cont-frac-iter
        (lambda (i) 1.0)
        (lambda (i) 1.0)
        8))
(good-enough? golden-ratio
    (cont-frac-iter
        (lambda (i) 1.0)
        (lambda (i) 1.0)
        9))