#lang racket
(require "p37-cont-frac.rkt")
; 当 (i+1) 取模 3 等于 0 时， Di 等于 (i+1)/3∗2
; 其他情况下， Di 返回 1
(define (e k)
    (define (N i) 1)
    (define (D i) 
        (if (= 0 (remainder (+ i 1) 3))
            (* 2 (/ (+ i 1) 3))
            1))
            
    (+ 2.0 
        (cont-frac-iter N D k)))



(e 1)
(e 2)
(e 5)
(e 10)
(e 100)