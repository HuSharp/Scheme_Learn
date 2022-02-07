#lang racket
; 判断 a 是否为 lat 的成员
;
(define member?
    (lambda (a lat) 
        (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat))
                            (member? a (cdr lat)))))))

; Examples of member? succeeding
;
(member? 'meat '(mashed potatoes and meat gravy))
(member? 'meat '(potatoes and meat gravy))
(member? 'meat '(and meat gravy))
(member? 'meat '(meat gravy))

; Examples of member? failing
(member? 'liver '(bagels and lox))
(member? 'liver '())
