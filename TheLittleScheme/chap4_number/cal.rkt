#lang racket
; Assume add1 is a primitive
;
(define add1
    (lambda (n)
        (+ n 1)))

; Example of add1
;
(add1 67)       ; 68

; Assume sub1 is a primitive
;
(define sub1
    (lambda (n)
        (- n 1)))

; Example of sub1
;
(sub1 5)        ; 4