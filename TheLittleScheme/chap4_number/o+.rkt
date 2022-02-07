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

; The o+ function adds two numbers
;
(define o+
    (lambda (m n)
        (cond 
            ((zero? n) m)       
            (else (add1 (o+ m (sub1 n)))))))

; Example of o+
;
(display "------ + ------\n")
(o+ 46 12)      ; 58


; The o- function subtracts one number from the other
;
(define o-
    (lambda (m n)
        (cond 
            ((zero? n) m)
            (else (sub1 (o- m (sub1 n))) ))))

; Example of o-
;
(display "------ - ------\n")
(o- 14 3)       ; 11
(o- 17 9)       ; 8

; The addtup function adds all numbers in a tup
;
(define addtup
    (lambda (tup)
        (cond 
            ((null? tup) 0)
            (else  (o+ (car tup) (addtup (cdr tup)))))))

; Examples of addtup
;
(display "------ addtup ------\n")
(addtup '(3 5 2 8))     ; 18
(addtup '(15 6 7 12 3)) ; 43

; The o* function multiplies two numbers
;
(define o*
    (lambda (m n)
        (cond 
            ((zero? n) 0)
            (else (o+ m (o* m (sub1 n))) ) )))

; Examples of o*
;
(display "------ o* -----\n")
(o* 5 3)                ; 15
(o* 13 4)               ; 52
