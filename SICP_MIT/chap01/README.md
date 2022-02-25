

选择 Lisp 的原因：计算过程的 Lisp 描述（称为过程）本身又可以作为 Lisp 的数据来表示和操作。而现存的很多程序设计技术，都依赖于填平“被动的”数据和“主动的”过程之间的传统划分。



### 正则序求值和应用序求值

正则序求值：完全展开 procedure 后再代入值

应用序求值：先代入参数而后应用（可以避免对表达式的重复求值）

### 函数与过程的矛盾

函数与过程的矛盾，表现在描述一件事情的特征，与描述如何去做这件事情之间的差异。

譬如平方根在数学中的定义往往是阐明是什么，但计算机中更关心的是如何去求出这个平方根。



### 递归与迭代

在 lec1b 中提到的栗子可以看到

迭代：time: O(x) space :O(1)

```scheme
; peano arithmetic
(define o+
    (lambda (x y) 
        (cond 
            ((= 0 x) y)
            (else (o+ (sub1 x) (add1 y)) ))))
```

递归：time: O(x) space :O(x)

```scheme
(define o+2
    (lambda (x y) 
        (cond 
            ((= 0 x) y)
            (else (add1 (o+2 (sub1 x) y)) ))))
```



递归过程和递归计算过程区分

上面的 `o+ ` 是递归过程的，但是将产生的是迭代的计算过程。迭代的计算过程指的是：在计算过程中的任意一点，那几个程序变量都提供了有关计算状态的一个完整描述。

> 当我们说一个过程是递归时，指的是语法形式上的事实，说明在这个过程的定义中，直接或间接的引用了该过程本身。
>
> 可以使用 [`(require racket/trace)`](https://docs.racket-lang.org/reference/debugging.html) 调用 `(trace fact-iter)` trace + 函数名 在解释器里面追踪两个 `plus` 函数的不同定义来考察它们所使用的计算模式

![image-20220222221050659](../assets/blog_image/README/image-20220222221050659.png)





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







