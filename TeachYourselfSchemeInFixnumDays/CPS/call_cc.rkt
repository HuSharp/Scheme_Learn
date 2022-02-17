#lang racket
(+ 1 (call/cc 
        (lambda (k) 
            (+ 2 (k 3)))))

(define r #f)
(+ 1 (call/cc
        (lambda (k) 
            (set! r k)
            (+ 2 (k 3)))))

(r 5)


(+ 4 (call/cc 
        (lambda (cc) 
            (cc (+ 1 2)))))


; ------------- “退出”续延 --------------
; 接收一个数字列表并把所有的数乘起来
(define list-product
    (lambda (s) 
        (let recur
            ((s s))
            (cond 
                ((null? s) 1)
                (else (* (car s) (recur (cdr s))))))))

(list-product '(6 1 2 3 4 5 6))
(list-product '(6 1 2 3 0 4 5 6))

; 在退出函数或循环时非常有用
; 使用 let 命名表达循环
(define list-product-exit
    (lambda (s) 
        (call/cc
            (lambda (exit) 
                (let recur
                    ((s s)) 
                (cond 
                    ((null? s) 1)
                    (else (cond 
                        ((= (car s) 0) (exit 0))
                        ((* (car s) (recur (cdr s))) )))))))))

(list-product-exit '(6 1 0 2 3 4 5 6))


(define (foo x)
    (call/cc (lambda (return)
                ;other stms
                (return (+ x x)))))