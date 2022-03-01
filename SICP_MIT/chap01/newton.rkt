#lang racket
(provide (all-defined-out))
(define (square x)
    (* x x))
; yn+1 = yn - f(yn) /  df/dy|(yn)
; 首先实现导数
; g(x+dx)-g(x) / dx
(define (deriv g)
    (lambda (x) 
        (/ (- (g (+ x dx)) (g x))
            dx)))
(define dx 0.000001)

; y=x^3 即 cube
; 经过求导之后 应该为 y=3x^2
(define (cube x)
    (* x x x))

((deriv cube) 5)            ; 75.00001501625775

; 现在再表示牛顿法
(define (newton-transform g)
    (lambda (x) 
        (- x (/ (g x) ((deriv g) x)))))

(require "fixed-point.rkt")
(define (newton g guess)
    (fixed-point (newton-transform g) guess))

(define (sqrt x)
    (newton (lambda (y) (- (square y) x))
            1.0))

(sqrt 100)