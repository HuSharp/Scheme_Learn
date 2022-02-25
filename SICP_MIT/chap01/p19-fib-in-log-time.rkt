#lang racket
; https://sicp.readthedocs.io/en/latest/chp1/19.html
; Tpq: 
;       a← bq+a(p+q)  
;       b← bp+aq 
; (Tpq)^2 : 
;       a← (bp+aq)q+(bq+a(p+q))(p+q)=b(2pq+q2)+a(p2+q2+2pq+q2),
;       b← (bp+aq)p+(bq+a(p+q))q=b(p2+q2)+a(2pq+q2).
(define fib
    (lambda (n) 
        (fib-iter 1 0 0 1 n)))          ; 即为置换

(require "sum-of-squares.rkt")
(define fib-iter
    (lambda (a b p q n) 
        (cond 
            ((= n 0) b)
            ((even? n) (fib-iter a              ; 此时采用 (Tpq)^2
                                b
                                (+ (square p) (square q))       ; new p
                                (+ (* 2 p q) (square q))        ; new q
                                (/ n 2)  ))              
            (else (fib-iter (+ (* b q) (* a q) (* a p))         ; new a
                            (+ (* b p) (* a q))                 ; new b
                            p
                            q
                            (- n 1))))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)