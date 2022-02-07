#lang racket
; The insertL function inserts the element new to the left of the first
; occurrence of element old in the list lat
;
(define insertL
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) (cons new lat))
            (else (cons (car lat)
                        (insertL new old (cdr lat)))))))

; Example of insertL
;
(insertL
    'f
    'g
    '(a b c d e g h))                    ; '(a b c d e f g h)
