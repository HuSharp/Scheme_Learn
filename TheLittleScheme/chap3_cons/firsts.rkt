#lang racket

; The firsts function builds a list of first s-expressions
;
(define firsts
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car (car l))
                        (firsts (cdr l)))))))

; Examples of firsts
;
(firsts '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant)))        ; '(apple plum grape bean)

(firsts '((a b) (c d) (e f)))                          ; '(a c e)
(firsts '((five plums) (four) (eleven green oranges))) ; '(five four eleven)
(firsts '(((five plums) four)
            (eleven green oranges)
            ((no) more)))           ; '((five plums) eleven (no))

