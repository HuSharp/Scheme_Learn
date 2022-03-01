#lang racket
(provide (all-defined-out))
(define average
    (lambda (x y) 
        (/ (+ x y) 2)))
; f(x)=x x称为函数 f 的不动点
(define (fixed-point f first-guess)
    (define (close-enough? x y)
        (< (abs (- x y)) 0.0001))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(fixed-point cos 1.0)
(fixed-point 
    (lambda (x) (+ (sin x) (cos x)))
    1.0)

; 此时我们发现 步骤和 sqrt.rkt 很像
; 可以将找平方根的计算过程转换为找不动点
; sqrt: y^2=x -> y=x/y -> 此时即找该函数的不动点
(define (sqrt x)
    (fixed-point
        (lambda (y) (/ x y))
        1.0))
; (sqrt 100)        ; 死循环

; 但问题在于推导可知会产生振荡
; 控制振荡的方式是不让猜测变化的太剧烈
; 因此此处使 y=x/y 猜测 1/2(x/y) 而非 (x/y)
(define (sqrt2 x)
    (fixed-point
        (lambda (y) (average y (/ x y)))
        1.0))

(sqrt2 100)

(require "sum-of-squares.rkt")
; 将过程作为返回值
(define (average-damp f)
    (lambda (x) (average x (f x))))
((average-damp square) 10)

(define (sqrt3 x)
    (fixed-point
        (average-damp 
            (lambda (y) (/ x y)))
        1.0))