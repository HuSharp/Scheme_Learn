#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

(define lat?
    (lambda (l) 
        (cond 
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

; Examples of lats:
;
(lat? '(Jack Sprat could eat no chicken fat))
(lat? '())
(lat? '(bacon and eggs))

; Examples of not-lats:
;
(lat? '((Jack) Sprat could eat no chicken fat)) ; not-lat because (car l) is a list
(lat? '(Jack (Sprat could) eat no chicken fat)) ; not-lat because l contains a list
(lat? '(bacon (and eggs)))                      ; not-lat because '(and eggs) is a list