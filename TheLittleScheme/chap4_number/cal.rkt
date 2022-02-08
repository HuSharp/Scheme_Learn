#lang racket
; Assume add1 is a primitive
;
(define add1
    (lambda (n)
        (+ n 1)))

; Example of add1
;
(add1 67)       ; 68

; Assume sub1 is a primitive
;
(define sub1
    (lambda (n)
        (- n 1)))

; Example of sub1
;
(sub1 5)        ; 4

; The o+ function adds two numbers
;
(define o+
    (lambda (m n)
        (cond 
            ((zero? n) m)       
            (else (add1 (o+ m (sub1 n)))))))

; Example of o+
;
(display "------ + ------\n")
(o+ 46 12)      ; 58


; The o- function subtracts one number from the other
;
(define o-
    (lambda (m n)
        (cond 
            ((zero? n) m)
            (else (sub1 (o- m (sub1 n))) ))))

; Example of o-
;
(display "------ - ------\n")
(o- 14 3)       ; 11
(o- 17 9)       ; 8

; The addtup function adds all numbers in a tup
;
(define addtup
    (lambda (tup)
        (cond 
            ((null? tup) 0)
            (else  (o+ (car tup) (addtup (cdr tup)))))))

; Examples of addtup
;
(display "------ addtup ------\n")
(addtup '(3 5 2 8))     ; 18
(addtup '(15 6 7 12 3)) ; 43

; The o* function multiplies two numbers
;
(define o*
    (lambda (m n)
        (cond 
            ((zero? n) 0)
            (else (o+ m (o* m (sub1 n))) ) )))

; Examples of o*
;
(display "------ o* -----\n")
(o* 5 3)                ; 15
(o* 13 4)               ; 52

; The tup+ function adds two tups
;
(define tup+
    (lambda (tup1 tup2)
        (cond 
            ((null? tup1) tup2) 
            ((null? tup2) tup1)
            (else (cons (o+ (car tup1) (car tup2)) 
                        (tup+ (cdr tup1) (cdr tup2))) ))))

; Examples of tup+
;
(display "------ tup+ ------\n")
(tup+ '(3 6 9 11 4) '(8 5 2 0 7))   ; '(11 11 11 11 11)
(tup+ '(3 7) '(4 6 8 1))            ; '(7 13 8 1)


; The o> function compares n with m and returns true if n>m
;
(define o>
    (lambda (m n)
        (cond 
            ((zero? m) #f)      ; 注意此处的 m n zero 判断过程顺序
            ((zero? n) #t)
            (else (o> (sub1 m) (sub1 n)) ))))

; Examples of o>
;
(display "------ o> ------\n")
(o> 12 133)     ; #f (false)
(o> 120 11)     ; #t (true)
(o> 6 6)        ; #f

; The o< function compares n with m and returns true if n<m
;
(define o<
    (lambda (m n)
        (cond 
            ((zero? n) #f)
            ((zero? m) #t)      ; 注意此处的 m n zero 判断过程顺序
            (else (o< (sub1 m) (sub1 n)) ))))

; Examples of o<
;
(display "------ o< ------\n")
(o< 4 6)        ; #t
(o< 8 3)        ; #f
(o< 6 6)        ; #f

; The o= function compares n with m and returns true if n=m
;
(define o=
    (lambda (m n)
        (cond 
            ((o> m n) #f)
            ((o< m n) #f)
            (else #t))))

; Examples of o=
;
(display "------ o= ------\n")
(o= 5 5)        ; #t
(o= 1 2)        ; #f

; The o^ function computes n^m
;
(define o^
    (lambda (m n)
        (cond 
            ((zero? n) 1)
            (else (o* m (o^ m (sub1 n))) ))))
        
; Examples of o^
;
(display "------ o^ ------\n")
(o^ 1 1)        ; 1
(o^ 2 3)        ; 8
(o^ 5 3)        ; 125


; The o/ function computes the integer part of n/m
; else 处 进行递归调用，第一个参数为当前 m 减去 n, 当递归返回时， 给返回结果加 1 
;
(define o/
    (lambda (m n)
        (cond 
            ((o< m n) 0)
            (else (add1 (o/ (o- m n) n)) ))))   

; Example of o/
;
(display "------ o/ ------\n")
(o/ 15 4)       ; 3
(o/ 17 4)       ; 4
