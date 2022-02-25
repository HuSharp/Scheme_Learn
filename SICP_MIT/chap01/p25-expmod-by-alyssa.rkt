#lang racket
(require "p16-expt-fast-iter.rkt")
(define expmod
    (lambda (base exp m) 
        (remainder (expt-fast base exp))))

; 解释如下：
; 因为费马检查在对一个非常大的数进行素数检测的时候，可能需要计算一个很大的乘幂，
; 比如说，求十亿的一亿次方，这种非常大的数值计算的速度非常慢，
; 而且很容易因为超出实现的限制而造成溢出。

; 而书本 34 页的 expmod 函数（即 fermat_prime.rkt 实现中的函数），
; 通过每次对乘幂进行 remainder 操作，
; 从而将乘幂限制在一个很小的范围内（不超过参数 m ），
; 这样可以最大限度地避免溢出，而且计算速度快得多。