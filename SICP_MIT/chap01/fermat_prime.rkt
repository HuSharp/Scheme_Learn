#lang racket
(provide (all-defined-out))
(require "sum-of-squares.rkt")
; 费马检查
; expmod 求出 base 的 exp幂 对 m 的取模结果
(define expmod
    (lambda (base exp m) 
        (cond 
            ((= exp 0) 1)
            ((even? exp) 
                (remainder (square (expmod base (/ exp 2) m))
                            m))
            (else (remainder (* base (expmod base (- exp 1) m))
                            m) ))))

(define fermat 
    (lambda (n) 
        (define try-it
            (lambda (a) 
                (= (expmod a n n) a)))
        (try-it (+ 1 (random (- n 1))))))

; times 表示给定几次检查，若每次都为对，那么为 true
(define fast-prime?
    (lambda (n times) 
        (cond 
            ((= times 0) #t)
            ((fermat n) (fast-prime? n (- times 1)))
            (else #f))))

(fast-prime? 97 5)
(fast-prime? 4 1)

(define fast-prime-most?
    (lambda (n) 
        (fast-prime? n 100)))