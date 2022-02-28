#lang racket
(require racket/trace)
(define SUM
    (lambda (TERM A NEXT B) 
        (if (> A B)
            0
            (+ (TERM A) 
                (SUM TERM (NEXT A) NEXT B)))))

;iter
(define SUM-ITER
    (lambda (TERM A NEXT) 
        (define (iter i ans)
            (if (> i b)
                ans
                (iter (NEXT i) 
                    (+ (TERM i) ans))))
    (iter A 0)))

(define add1
    (lambda (x) 
        (+ x 1)))

(define SUM-INT
    (lambda (A B) 
        (define (identity A) A) 
        (SUM identity A add1 B)))

(require "../sum-of-squares.rkt")
(define SUM-SQ
    (lambda (A B) 
        (define (identity A) (square A))
        (SUM identity A add1 B)))

; 收敛到 π/8
(define PI-SUM
    (lambda (A B) 
        (SUM (lambda (i) (/ 1.0 (* i (+ i 2))))
            A (lambda (x) (+ x 4)) 
            B)))

(SUM-INT 1 9)
(SUM-SQ 1 9)
(* 8 (PI-SUM 1 1000))