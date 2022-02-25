#lang racket
(define sub1
    (lambda (x) 
        (- x 1)))

; 迭代版本
(define move
    (lambda (N from to spare) 
        (cond 
            ((0 = N) 'Done)
            (else (move (sub1 N) from spare to)
                    (display 'move from to)
                    (move (sub1 N) spare to from) ))))


(move 4 1 2 3)