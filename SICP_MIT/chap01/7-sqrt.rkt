#lang racket
#lang racket
(define square
    (lambda (x) 
        (* x x)))

(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define improve
    (lambda (guess x) 
        (average guess (/ x guess))))

(define good-enough?
    (lambda (guess x) 
        (< (abs (- (square guess) x)) 0.001)))


(define sqrt-iter
    (lambda (guess x) 
        (cond 
            ((good-enough? guess x) guess)
            (else (sqrt-iter (improve guess x)
                        x) ))))

(define sqrt
    (lambda (x) 
        (sqrt-iter 1.0 x)))

; 对 good-enough? 进行修改：不再检测猜测值 guess 的平方与 x 之间的差，
; 而是检测新旧两次猜测值之间的比率，当比率变化非常小时，程序就停止 improve 。
(define good-enough2?
    (lambda (old_guess new_guess) 
        (> 0.01 
            (/ (abs (- new_guess old_guess))
                old_guess))))

(define sqrt-iter2
    (lambda (guess x) 
        (cond 
            ((good-enough2? guess (improve guess x)) (improve guess x))
            (else (sqrt-iter2 (improve guess x)
                        x) ))))

(define sqrt2
    (lambda (x) 
        (sqrt-iter2 1.0 x)))

(display "------------- old sqrt -------------")
(sqrt 9)
(sqrt 100)
(sqrt 2)
; practice 1.7 small
;Value: .03220324381282134  
(sqrt 0.00009)  ; 正确答案应该是 9.486832980505138e-3
; practice 1.7 big
; (sqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000) 
(display "------------- new sqrt -------------")
(sqrt2 9)
(sqrt2 100)
(sqrt2 2)
; practice 1.7 small
;Value: .03220324381282134  
(sqrt2 0.00009)  ; 正确答案应该是 9.486832980505138e-3
; practice 1.7 big
(sqrt2 900000000000000000000000000000000000000000000000000000000000000000000000000000000000) 