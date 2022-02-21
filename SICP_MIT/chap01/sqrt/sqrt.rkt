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

(display "------------- old sqrt -------------")
(sqrt 9)
(sqrt 100)
(sqrt 2)
; practice 1.7 small
;Value: .03220324381282134  
(sqrt 0.00009)  ; 正确答案应该是 9.486832980505138e-3
; practice 1.7 big
; (sqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000) 
