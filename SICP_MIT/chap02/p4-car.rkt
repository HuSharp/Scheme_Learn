#lang racket
(define (cons2 x y)
    (lambda (m) 
        (m x y)))

(define (car2 z)
    (z (lambda (p q) p)))

; (car (cons 1 2))

; (car (lambda (m) (m 1 2)))          ; 展开 cons

; ((lambda (m) (m 1 2))               ; 代换 m
;     (lambda (p q) p))

; ((lambda (p q) p)                   ; 代换 p
;     1 2)

; 1


(define (cdr2 z)
    (z (lambda (p q) q)))

(car2 (cons2 3 4))
(cdr2 (cons2 3 4))