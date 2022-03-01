#lang racket
(require "p43-repeated.rkt")
(require "fixed-point.rkt")
(define (expt base n)
    (if (= n 0)
        1
        ((repeated (lambda (x) (* base x)) n) 1)))
(expt 2 10)

(define (damp-n-times n damp-times)
    (lambda (x) 
        (fixed-point 
            (average-damp-n-times
                (lambda (y) 
                    (/ x (expt y (- n 1))))
                damp-times)
        1.0)))

; 进行任意次平均阻尼变换的 average-damp-n-times 
(define (average-damp-n-times f times)
    ((repeated average-damp times) f))
; 平均阻尼
(define (average-damp f)
    (lambda (x) (average x (f x))))

; 通过定义平方根、立方根和四次方根来测试 damp-n-times
(define sqrt (damp-n-times 2 1))
(sqrt (* 3 3))

(define cube (damp-n-times 3 1))
(cube (* 4 4 4))

(define 4th (damp-n-times 4 2))
(4th (* 5 5 5 5))
; 至此 验证函数成功，需要进行收敛实验，以下为部分实验数据
; n 次方根              1	2	3	4	5	6	7	8	...	15	16	...	31	32	...
; 收敛所需的平均阻尼次数    1	1	1	2	2	2	2	3	...	3	4	...	4	5	...
; 可以看出，要使得计算 n 次方根的不动点收敛，最少需要 ⌊lgn⌋ 次平均阻尼。
(define (lg n)
    (cond 
        ((> (/ n 2) 1)
            (+ 1 (lg (/ n 2))))
        ((< (/ n 2) 1)
            0)
        (else 1)))

; 因此，最终的收敛次数结合 lg 和 damp-n-times
(define (nth n)
    (damp-n-times n (lg n)))

(define sqrtN (nth 2))
(sqrt (* 3 3))

(define cubeN (nth 3))
(cube (* 4 4 4))

(define 4thN (nth 4))
(4th (* 5 5 5 5))

(define 100thN (nth 100))
(100thN 100)