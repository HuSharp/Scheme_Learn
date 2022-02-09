; (define subst*
;     (lambda (new old lat) 
;         (cond 
;             ((null? lat) '())
;             ((atom? (car lat))
;                 (cond 
;                     ((eq? (car lat) old) 
;                         (cons new 
;                             (subst* new old (cdr lat)) ))
;                     (else (cons old
;                             (subst* new old (cdr lat))))))
;             (else (cons (subst* new old (car lat)) 
;                         (subst* new old (cdr lat)) )))))