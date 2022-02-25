#lang racket
(require racket/trace)
(define cube
    (lambda (x) 
        (* x x x)))

(define p 
    (lambda (x) 
        (- (* 3 x) (* 4 (cube x)))))

(define sine
    (lambda (angle) 
        (cond 
            ((not (> (abs angle) 0.1)) angle)
            (else (p (sine (/ angle 3)))))))
(trace sine)           ; 可看到 时间和空间复杂度都是 O(loga) 。
(sine 12.15)        ; p 将被使用