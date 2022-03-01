#lang racket
(define dx 0.000001)
(define (smooth f)
    (lambda (x) 
        (/ (+ (f (- x dx))
                (f x)
                (f (+ x dx)))
        3)))
(define (square x)
    (* x x))
((smooth square) 5)


(define (smooth-n-times f n)
    (if (= n 0)
        f
        (smooth (smooth-n-times f (- n 1)))))
; iter
(define (smooth-n-times f n)
    (define (iter i smooth-f)
        (if (= i 0)
            smooth-f
            (iter (- i 1)
                (smooth smooth-f))))
    (iter n f))

; repeated
(require "p43-repeated.rkt")
(define (smooth-times f n)
    (let ((n-times (repeated smooth n)))
        (n-times f)))

((smooth-times square 10) 5)

