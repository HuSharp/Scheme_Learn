#lang racket
; The insertR function inserts the element new to the right of the first
; occurence of element old in the list lat
;
(define insertR
    (lambda (new old lat) 
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) (cons old 
                                    (cons new (cdr lat)))) 
            (else (cons (car lat) 
                        (insertR new old (cdr lat)) )))))

; Examples of insertR
;
(insertR
    'topping 'fudge
    '(ice cream with fudge for dessert)) ; '(ice cream with fudge topping for dessert)

(insertR
    'jalapeno
    'and
    '(tacos tamales and salsa))          ; '(tacos tamales and jalapeno salsa)

(insertR
    'e
    'd
    '(a b c d f g h))                  ; '(a b c d e f g h)
