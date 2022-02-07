#lang racket
; The multisubst function substitutes all occurence of element old with new
; in the list lat
;
(define multisubst
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) 
                (cons new
                    (multisubst new old (cdr lat)) ))
            (else (cons (car lat) 
                    (multisubst new old (cdr lat)) ))) ))

; Example of multisubst
;
(multisubst
    'x
    'a
    '(a b c d e a a b))  ; (x b c d e x x b)
