#lang racket
; 从 lat 中移除 a
(define rember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? (car lat) a) (cdr lat))
                    (else (rember a (cdr lat))))))))

; cons 构建的第二版本 rember
(define rember2
    (lambda (a lat) 
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? (car lat) a) (cdr lat))
                    (else (cons (car lat) 
                                (rember2 a (cdr lat)))))))))

(rember 1 '(1 2 3))
(rember "a" '("a" "b" "c"))

(rember2 1 '(1 2 3))
(rember2 "a" '("a" "b" "c"))