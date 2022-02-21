

选择 Lisp 的原因：计算过程的 Lisp 描述（称为过程）本身又可以作为 Lisp 的数据来表示和操作。而现存的很多程序设计技术，都依赖于填平“被动的”数据和“主动的”过程之间的传统划分。



### 正则序求值和应用序求值

正则序求值：完全展开 procedure 后再代入值

应用序求值：先代入参数而后应用（可以避免对表达式的重复求值）



函数与过程的矛盾，表现在描述一件事情的特征，与描述如何去做这件事情之间的差异。

譬如平方根在数学中的定义往往是阐明是什么，但计算机中更关心的是如何去求出这个平方根。





```
#lang racket
(define (square x)
    (* x x))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x))
            0.001))


(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))


(define (sqrt x)
    (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 137)
```







