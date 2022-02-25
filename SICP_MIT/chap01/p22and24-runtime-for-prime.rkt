#lang racket
(require "prime.rkt")
(require "fermat_prime.rkt")
(require "p23-new-prime.rkt")

(define timed-prime-test
    (lambda (n) 
        (newline)
        (display n)
        (start-prime-test n (current-inexact-milliseconds))))

(define start-prime-test
    (lambda (n start-time) 
        (cond (prime? n) 
            (report-prime (- (current-inexact-milliseconds) start-time)))))

(define report-prime
    (lambda (elapsed-time) 
        (display " **** " )
        (display elapsed-time)))

; 以下参考： https://sicp.readthedocs.io/en/latest/chp1/22.html
; generate odd
(define next-odd
    (lambda (x) 
        (cond 
            ((odd? x) (+ x 2))
            (else (+ x 1) ))))
(next-odd 6)        ; 7
(next-odd 7)        ; 9

; 生成连续素数的函数就很直观了：
; 首先使用 next-odd 生成一个奇数，然后使用 prime? 
; 检查给定的奇数是否素数，一直这样计算下去，直到满足参数 count 指定的数量为止。
(define generate-continue-prime
    (lambda (n num prime-algo) 
        (cond 
            ((= num 0) (display "generate primes finished.\n"))
            ((prime-algo n) 
                (display n)
                (newline)
                (generate-continue-prime (next-odd n) (- num 1) prime-algo))
            (else (generate-continue-prime (next-odd n) num prime-algo) ))))

(display "--------- generate-continue-prime ---------\n")
(generate-continue-prime 1000 3 prime?)
(generate-continue-prime 1000 10 prime?)
(generate-continue-prime 10000 3 prime?)

; 需要检查范围内的连续奇数的素性
(define search-for-primes
    (lambda (small nums prime-algo)
        (let ((start-time (current-inexact-milliseconds)))
            (display "search for primes, start by ")
            (display small)
            (display " and num is: ")
            (display nums)
            (newline)
            (generate-continue-prime small nums prime-algo)
            (- (current-inexact-milliseconds) start-time))) )

(display "--------- search-for-primes ---------\n")
(search-for-primes 1000 3 prime?)                   ; 0.028564453125
(search-for-primes 10000 3 prime?)                  ; 0.031005859375
(search-for-primes 100000 3 prime?)                 ; 0.05029296875
(search-for-primes 1000000 3 prime?)                ; 0.10107421875

(search-for-primes 1000 3 fast-prime-most?)         ; 0.326416015625
(search-for-primes 10000 3 fast-prime-most?)        ; 0.383056640625
(search-for-primes 100000 3 fast-prime-most?)       ; 0.42138671875
(search-for-primes 1000000 3 fast-prime-most?)      ; 0.445068359375