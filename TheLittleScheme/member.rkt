#lang racket
; 判断 a 是否为 lat 的成员
(define member?
    (lambda (a lat) 
        (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat))
                            (member? a (cdr lat)))))))

(member? 1 '(1 2 3))
(member? "a" '("a" "b" "c"))