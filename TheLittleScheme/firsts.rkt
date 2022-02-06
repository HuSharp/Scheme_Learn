#lang racket

; cons 构建的第二版本 rember
(define firsts
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car (car l))
                        (firsts (cdr l)))))))

(firsts '((apple peach pumpkin)
            (plum pear cherry)
            (grape raisin pea)
            (bean carrot eggplant))) 

