#lang racket
; The subst function substitutes the first occurence of element old with new
; in the list lat
;
(define subst
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) (cons new (cdr lat)))
            (else (cons (car lat) 
                    (subst new old (cdr lat))) ))))

; Example of subst
;
(subst
    'topping
    'fudge
    '(ice cream with fudge for dessert)) ; '(ice cream with topping for dessert)

; The subst2 function substitutes the first occurence of elements o1 or o2
; with new in the list lat
;
(define subst2
    (lambda (new o1 o2 lat)
        (cond 
            ((null? lat) '())
            ((or (eq? (car lat) o1) (eq? (car lat) o2)) 
                (cons new (cdr lat)))
            (else (cons (car lat) 
                    (subst2 new o1 o2 (cdr lat))) ))))

; Example of subst2
;
(subst2
    'vanilla
    'chocolate
    'banana
    '(banana ice cream with chocolate topping))  ; '(vanilla ice cream with chocolate topping)
