#lang racket
; A different number representation:
; - 选 `()` 表示 0
; - 选 `(())` 表示 1
; - 选 `(()())` 表示 2
; - 选 `(()()())` 表示 3

; sero? just like zero?
;
(define sero? 
    (lambda (n) 
        (null? n)))
; edd1 just like add1
;
(define edd1
    (lambda (n) 
        (cons '() n)))
; zub1 just like sub1
;
(define zub1
    (lambda (n) 
        (cdr n)))

; .+ just like o+
;
(define .+
    (lambda (m n)
        (cond 
            ((sero? n) m)
            (else (edd1 (.+ m (zub1 n))) ))))
; Example of .+
;
(.+ '(()) '(() ()))     ; (+ 1 2)
;==> '(() () ())



(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
; tat? just like lat?
;
(define tat?
    (lambda (lat)
        (cond 
            ((sero? lat) #t)
            ((atom? (car lat)) (tat? (cdr lat)) )
            (else #f))))
; But does tat? work

(tat? '((()) (()()) (()()())))  ; (lat? '(1 2 3))
; ==> #f

; Beware of shadows.
; 这就大错特错了，因此在进行高层次抽象时，要小心错误冷不丁的从阴影冒出。