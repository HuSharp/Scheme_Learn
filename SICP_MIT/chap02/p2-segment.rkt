#lang racket
(provide (all-defined-out))
; 进行线段的表示
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

(define (make-point x y)
    (cons x y))
(define (x-point p)
    (car p))
(define (y-point p)
    (cdr p))

;segment
(define (make-segment start end)
    (cons start end))
(define (start-segment s)
    (car s))
(define (end-segment s)
    (cdr s))

(define (average x y)
    (/ (+ x y) 2.0))

(define (midpoint-segment s)
    (let ((start (start-segment s))
            (end (end-segment s)))
        (cons (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)) )))

(define start (make-point 1 3))
(define end (make-point 4 3))
(define seg (make-segment start end))
(define mid (midpoint-segment seg))
(print-point mid)