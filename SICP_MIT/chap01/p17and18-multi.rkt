#lang racket
(require racket/trace)
(define double
    (lambda (n) 
        (* n 2.0)))

(define halve
    (lambda (n) 
        (/ n 2.0)))

; rec
(define multi-rec
    (lambda (a b) 
        (cond 
            ((= 0 b) 0)
            ((even? b) (multi-rec (double a) (halve b)))
            (else (multi-rec a (- b 1)) ))))

(display "------------ multi-rec ------------\n")
(trace multi-rec)
(multi-rec 5 9)
(multi-rec 5 8)

; iter
(define multi
    (lambda (a b) 
        (multi-iter a b 0)))

(define multi-iter
    (lambda (a b product) 
        (cond 
            ((= 0 b) product)
            ((even? b) (multi-iter (double a) (halve b) product))
            (else (multi-iter a (- b 1) (+ product a)) ))))
(display "------------ multi-iter ------------\n")
(trace multi-iter)
(multi 5 9)
(multi 5 8)