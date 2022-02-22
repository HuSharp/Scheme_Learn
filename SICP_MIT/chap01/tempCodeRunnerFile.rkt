#lang racket
; 输出帕斯卡三角形的指定位置的元素
; rec
(define pascal
    (lambda (row col) 
        (cond 
            ((> col row) (error "unvalid col value"))
            ((or (= col 0) (= row col)) 1)
            (else (+ (pascal (- row 1) (- col 1))
                        (pascal (- row 1) col)) ))))

(display "----------- pascal ---------\n")
(pascal 4 0)    ;1
(pascal 4 4)    ;1
(pascal 4 2)    ;6

; pascal iter
(require "factorial.rkt")
; row! / col!*(row−col)! ，
(define pascal-iter
    (lambda (row col) 
        (/ (factorial row)
            (* (factorial col)
                (factorial (- row col))))))

(display "----------- pascal-iter ---------\n")
(pascal-iter 4 0)    ;1
(pascal-iter 4 4)    ;1
(pascal-iter 4 2)    ;6
