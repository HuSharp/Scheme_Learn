#lang racket
; rec
(define f
    (lambda (n) 
        (cond 
            ((< n 3) n)
            (else (+ (f (- n 1)) 
                    (* 2 (f (- n 2)))
                    (* 3 (f (- n 3)))) ))))
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)

; iter
(define f2
    (lambda (n) 
        (f-iter 2 1 0 0 n)))

(define f-iter
    (lambda (a b c i n) 
        (cond 
            ((= i n) c)             ; return last ， keep >>
            (else (f-iter (+ a (* 2 b) (* 3 c))     ; new a
                            a                       ; new b 
                            b                       ; new c
                            (+ i 1)
                            n) ))))
(f2 1)
(f2 2)
(f2 3)
(f2 4)
(f2 5)