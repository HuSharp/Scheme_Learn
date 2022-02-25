#lang racket
(require "sum-of-squares.rkt")
(define expmod
    (lambda (base exp m) 
        (cond 
            ((= exp 0) 1)
            ((nontrival-square-root? base m) )      ; 增加对非平凡根的检测
            ((even? exp) 
                (remainder (square (expmod base (/ exp 2) m))
                            m))
            (else (remainder (* base (expmod base (- exp 1) m))
                            m) ))))

; 看看是否有一个数 a ，它既不等于 1 ，也不等于 n−1 ，但它的平方取模 n 等于 1 ：
(define nontrival-square-root?
    (lambda (a n) 
        (and (not (= a 1))
            (not (= a (- n 1)))
            (= 1 (remainder (square a) n)) )))

; 接受一个参数 n ，生成大于 0 小于 n 的随机数
(define non-zero-random
    (lambda (n) 
        (let ((r (random n)))
            (if (not (= r 0))
                r
                (non-zero-random n)))))

; 计算 an−1 时只有一半的几率会遇到 1 取模 n 的非平凡方根，
; 因此我们至少要执行测试 n/2 次才能保证测试结果的准确性
; 使用了 ceiling 函数对 n/2 的值进行向上取整
(define miller-rabin-test
    (lambda (n) 
        (let ((times (ceiling (/ n 2))))
            (test-iter n times))))

(define test-iter
    (lambda (n times) 
        (cond 
            ((= times 0) #t)
            ((= (expmod (non-zero-random n) (- n 1) n) 1) 
                (test-iter n (- times 1)))
            (else #f))))


(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)

(miller-rabin-test 2)
(miller-rabin-test 3)
(miller-rabin-test 11)
(miller-rabin-test 17)
(miller-rabin-test 43)
(miller-rabin-test 97)