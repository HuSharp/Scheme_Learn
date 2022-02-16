#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

; The rember-f function takes the test function, element, and a list
; and removes the element that test true
;
(define rember-f
    (lambda (test? a lat) 
        (cond 
            ((null? lat) '())
            ((test? a (car lat)) (cdr lat))
            (else (cons (car lat)
                        (rember-f test? a (cdr lat))) ))))

; Examples of rember-f
;
(rember-f eq? 2 '(1 2 3 4 5))
; ==> '(1 3 4 5)

; The eq?-c function takes an atom and returns a function that
; takes an atom and tests if they are the same
;
(define eq?-c
    (lambda (x) 
        (lambda (a) 
            (eq? x a))))