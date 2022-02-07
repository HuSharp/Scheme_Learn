#lang racket
; The multirember function removes all occurances of a from lat
;
(define multirember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) a) (multirember a (cdr lat)))   ;相等则 skip
            (else (cons (car lat) 
                        (multirember a (cdr lat)))) )))

; Example of multirember
;
(multirember
    'cup
    '(coffee cup tea cup and hick cup))    ; '(coffee tea and hick)
