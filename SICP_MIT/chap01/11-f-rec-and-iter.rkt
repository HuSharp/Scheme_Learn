#lang racket
; rec
(define f
    (lambda (n) 
        (cond 
            ((< n 3) n)
            (else (+ (f (- n 1)) (* (f (- n 2)) 2)
                    (* 3 (f (- n 3)))) ))))

(f 4)

; iter
(define f2
    (lambda (n) 
        (f-iter 2 1 0 0 n)))

(define f-iter
    (lambda (a b c i n) 
        (cond 
            ((= i n) c)
            (else (f-iter (+ a (* 2 b) (* 3 c))     ; new a
                            a                       ; new b 
                            b                       ; new c
                            (+ i 1)
                            n) ))))
(f2 4)
