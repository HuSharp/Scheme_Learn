#lang racket
(define multiInsertR
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) (cons old 
                                    (cons new 
                                        (multiInsertR new old (cdr lat))))) 
            (else (cons (car lat) 
                    (multiInsertR new old (cdr lat)) )))))

; Example of multiinsertR
;
(multiInsertR
    'x
    'a
    '(a b c d e a a b))  ; (a x b c d e a x a x b)


(define multiInsertL
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) (cons new 
                                    (cons old
                                        (multiInsertL new old (cdr lat)))))
            (else (cons (car lat)
                    (multiInsertL new old (cdr lat)) )))))

; Example of multiinsertL
;
(multiInsertL
    'x
    'a
    '(a b c d e a a b))  ; (x a b c d e x a x a b)