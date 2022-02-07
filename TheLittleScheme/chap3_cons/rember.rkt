#lang racket
; 从 lat 中移除 a  
; 以下版本存在一个问题：会丢失掉删除元素之前的所有元素
;
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

; Examples of rember function
;
(rember 'mint '(lamb chops and mint flavored mint jelly)) ; '(lamb chops and flavored mint jelly)
(rember 'toast '(bacon lettuce and tomato))               ; '(bacon lettuce and tomato)
(rember 'cup '(coffee cup tea cup and hick cup))          ; '(coffee tea cup and hick cup)

; Examples of rember2 function
;
(rember2 'mint '(lamb chops and mint flavored mint jelly)) ; '(lamb chops and flavored mint jelly)
(rember2 'toast '(bacon lettuce and tomato))               ; '(bacon lettuce and tomato)
(rember2 'cup '(coffee cup tea cup and hick cup))          ; '(coffee tea cup and hick cup)