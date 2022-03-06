#lang racket
(define zero
    (lambda (f) 
        (lambda (x) x)))

(define (add-1 n)
    (lambda (f) 
        (lambda (x) 
            (f ((n f)) x))))

; one 的推导如下：
(add-1 zero)
; ->
(add-1 (lambda (f) 
        (lambda (x) x)))
; ->
(lambda (f)
    (lambda (x) 
        (f                  ; 此处代入 zero
            ((lambda (f)
                (lambda (x)
                    x))
            f)
        x)))
; ->
(lambda (f)
    (lambda (x) 
        (f
            (lambda (x) x)
        x)))
; ->
(lambda (f)
    (lambda (x) 
        (f x)))            ; 此即为 one
; ->
(define one 
    (lambda (f)
        (lambda (x) 
            (f x))))


; two的推导
(add-1 one)
; ->
(add-1 (lambda (f)
        (lambda (x) 
            (f x))))
; ->
(lambda (f) 
        (lambda (x) 
            (f 
                ((lambda (f)
                    (lambda (x) 
                        (f x))) 
                f) x)))
; ->
(lambda (f) 
        (lambda (x) 
            (f 
                ((lambda (x) 
                        (f x))
                ) x)))
; ->
(lambda (f) 
    (lambda (x) 
        (f (f x))))
; ->
(define two
    (lambda (f) 
        (lambda (x) 
            (f (f x)))))

; 现在总结对比发现
(define zero
    (lambda (f)
        (lambda (x)
            x)))            ; 没有 f

(define one
    (lambda (f)
        (lambda (x)
            (f x))))        ; 一个 f 调用

(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))))    ; 两个 f 调用

; 由此推导 即为增加 f
; 因此 three 和 four 的定义很可能是：
(define three
    (lambda (f)
        (lambda (x)
            (f (f (f x))))))        ; 三个 f 调用

(define four
    (lambda (f)
        (lambda (x)
            (f (f (f (f x)))))))    ; 四个 f 调用

; 我们就得出了 Church 计数表示(非负)整数的一般规则：
; 从 zero 的定义开始，每次数值加一时，函数体内都会增加一个(嵌套的) f 函数的调用；
; 当两个 Chruch 数相加时，它们的和就等于累积起两个过程中的 f 调用。
(+ 3 2)
; ->
(+ (lambda (f)
        (lambda (x)
            (f (f (f x)))))
    (lambda (f)
        (lambda (x)
            (f (f x)))))
; ...
(lambda (f)
    (lambda (x)
        (f (f (f (f (f x)))))))

; 现在给出 + 的定义
; 思考 add-1
(define (add-1 n)
    (lambda (f) 
        (lambda (x) 
            (f ((n f)) x))))

(define +
    (lambda (m) 
        (lambda (n) 
            (lambda (f) 
                (lambda (x) 
                    (m f (n f x)))))))