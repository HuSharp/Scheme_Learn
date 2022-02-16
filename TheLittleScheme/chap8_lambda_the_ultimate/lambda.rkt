#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

; The rember-f function takes the test function, element, and a list
; and removes the element that test true
;
(define rember-f
    (lambda (test? a lat) 
        (cond 
            ((null? lat) '())
            ((test? a (car lat)) (cdr lat))
            (else (cons (car lat)
                        (rember-f test? a (cdr lat))) ))))

; Examples of rember-f
;
(rember-f eq? 2 '(1 2 3 4 5))
; ==> '(1 3 4 5)

; The eq?-c function takes an atom and returns a function that
; takes an atom and tests if they are the same
;
(define eq?-c
    (lambda (a)             ; currying
        (lambda (x)
            (eq? x a))))
; Example of eq?-c
;
(display "------ eq?-c ------\n")
((eq?-c 'tuna) 'tuna)       ; #t
((eq?-c 'tuna) 'salad)      ; #f

(display "------ eq?-salad ------\n")
(define eq?-salad (eq?-c 'salad))

; Examples of eq?-salad
;
(eq?-salad 'salad)          ; #t
(eq?-salad 'tuna)           ; #f

; Another version of rember-f that takes test as an argument
; and returns a function that takes an element and a list
; and removes the element from the list
;
(define rember-f-currying
    (lambda (test?) 
        (lambda (a lat) 
            (cond 
                ((null? lat) '())
                ((test? a (car lat)) (cdr lat))
                (else (cons (car lat)
                            ((rember-f-currying test?) a (cdr lat))) )))))
        
; Test of rember-f-currying
;
(display "------ rember-f-currying ------\n")
((rember-f-currying eq?) 2 '(1 2 3 4 5))
; ==> '(1 3 4 5)

; Curry (rember-f eq?)
;
(define rember-eq? (rember-f-currying eq?))

; Test curried function
;
(rember-eq? 2 '(1 2 3 4 5))
; ==> '(1 3 4 5)
(rember-eq? 'tuna '(tuna salad is good))
; ==> '(salad is good)
(rember-eq? 'tuna '(shrimp salad and tuna salad))
; ==> '(shrimp salad and salad)
(rember-eq? 'eq? '(equal? eq? eqan? eqlist? eqpair?))
; ==> '(equal? eqan? eqlist? eqpair?)


; The insertL function from Chapter 3 (03-cons-the-magnificent.ss)
; This time curried
;
(define insertL-f
    (lambda (test?) 
        (lambda (new old lat) 
            (cond 
                ((null? lat) '())
                ((test? old (car lat))
                    (cons new 
                        (cons old 
                            (cdr lat))))
                (else (cons (car lat)
                            ((insertL-f test?) new old (cdr lat))) )))))

; Test insertL-f
;
(display "------ insertL-f & insertR-f  ------\n")
((insertL-f eq?)
    'd
    'e
    '(a b c e f g d h))                  ; '(a b c d e f g d h)

; The insertR function, curried
;
(define insertR-f
    (lambda (test?) 
        (lambda (new old lat) 
            (cond 
                ((null? lat) '())
                ((test? old (car lat))
                    (cons old
                        (cons new  
                            (cdr lat))))
                (else (cons (car lat)
                            ((insertR-f test?) new old (cdr lat))) )))))

; Test insertR-f
((insertR-f eq?)
    'e
    'd
    '(a b c d f g d h))                  ; '(a b c d e f g d h)


; The seqL function is what insertL does that insertR doesn't
;
(define seqL
    (lambda (new old lat) 
        (cons new (cons old lat))))

; The seqR function is what insertR does that insertL doesn't
;
(define seqR
    (lambda (new old lat) 
        (cons old (cons new lat))))

; insert-g acts as insertL or insertR depending on the helper
; function passed to it
;
(define insert-g
    (lambda (seq) 
        (lambda (new old lat)
            (cond 
                ((null? lat) '())
                ((eq? old (car lat))
                    (seq new old (cdr lat)))
                (else (cons (car lat)
                            ((insert-g seq) new old (cdr lat))) )))))

; insertL is now just (insert-g seqL)
;
(define insertL (insert-g seqL))

; insertR is now just (insert-g seqR)
;
(define insertR (insert-g seqR))
; Test insertL
;
(display "------ insertL & insertR ------\n")
(insertL
    'd
    'e
    '(a b c e f g d h))                  ; '(a b c d e f g d h)

; Test insertR
(insertR
    'e
    'd
    '(a b c d f g d h))                  ; '(a b c d e f g d h)

; it can also impl by using lambda
(define insertL-lambda
    (insert-g 
        (lambda (new old lat) 
            (cons new (cons old lat)))))
; Test insertL
;
(insertL-lambda
    'd
    'e
    '(a b c e f g d h))                  ; '(a b c d e f g d h)

; The subst function from Chapter 3 (03-cons-the-magnificent.ss)
;
(define subst
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            ((eq? (car lat) old) 
                (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst new old (cdr lat))) ))))
; Test subst
;
(display "------ subst ------\n")
(subst
    'topping
    'fudge
    '(ice cream with fudge for dessert)) ; '(ice cream with topping for dessert)

; The seqS function is what subst does that neither insertL nor insertR do
;
(define seqS
    (lambda (new old lat) 
        (cons new lat)))

(define subst-f (insert-g seqS))
; Test subst-f
;
(display "------ subst-f ------\n")
(subst-f
    'topping
    'fudge
    '(ice cream with fudge for dessert)) ; '(ice cream with topping for dessert)


(define seqrem
    (lambda (new old lat) lat))

(define rember
    (lambda (a lat) 
        ((insert-g seqrem) #f a lat)))


; It's rember! Let's test it.
;
(display "------ rember ------\n")
(rember
    'sausage
    '(pizza with sausage and bacon))      ; '(pizza with and bacon)


; Remember the value function from Chapter 6 (06-shadows.ss)?
; The value function determines the value of an arithmetic expression
;
(define value
    (lambda (nexp) 
        (cond 
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) 'o+)
                (+ (value (car nexp))
                    (value (car (cdr (cdr nexp))))))
            ((eq? (car (cdr nexp)) 'ox)
                (* (value (car nexp))
                    (value (car (cdr (cdr nexp))))))
            (else 
                (expt (value (car nexp))
                    (value (car (cdr (cdr nexp)))))))))
; Let's abstract it
;
(define atom-to-func
    (lambda (atom)
        (cond 
            ((eq? atom 'o+) +)
            ((eq? atom 'ox) *)
            ((eq? atom 'o^) expt)
            (else #f))))

; atom-to-function uses operator
;
(define operator
    (lambda (aexp) 
        (car aexp)))
; Example of atom-to-function
;
(atom-to-func (operator '(o+ 5 3)))     ; #<procedure:+>
; value uses 1st-sub-exp
;
(define 1st-sub-exp
    (lambda (aexp) 
        (car (cdr aexp))))
; value uses 2nd-sub-exp
(define 2nd-sub-exp
    (lambda (aexp) 
        (car (cdr (cdr aexp)))))

; The value function rewritten to use abstraction
;
(define value-f
    (lambda (nexp)
        (cond 
            ((atom? nexp) nexp)
            (else 
                ((atom-to-func (operator nexp))
                    (value-f (1st-sub-exp nexp))
                    (value-f (2nd-sub-exp nexp)))))))

; Test value
;
(display "------ value-f ------\n")
(value-f 13)                                   ; 13
(value-f '(o+ 1 3))                            ; 4
(value-f '(o+ 1 (o^ 3 4)))                     ; 82


; The multirember function from Chapter 3 (03-cons-the-magnificent.ss)
;
(define multirember-f
    (lambda (test?) 
        (lambda (a lat) 
            (cond 
                ((null? lat) '())
                ((test? a (car lat)) 
                    ((multirember-f test?) a (cdr lat)))
                (else (cons (car lat)
                            ((multirember-f test?) a (cdr lat))))))))
; Test multirember-f
;
(display "------ multirember-f ------\n")
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
; ==> '(shrimp salad salad and)

(define multirember-eq? (multirember-f eq?))

; eq?-tuna tests if element is equal to 'tuna
;
(define eq?-tuna
    (eq?-c 'tuna))
; The multiremberT changes the way test works
;
(define multiremberT
    (lambda (test? lat) 
        (cond 
            ((null? lat) '())
            ((test? (car lat))          ; 此 test 只判断一个参数
                (multiremberT test? (cdr lat)))
            (else (cons (car lat)
                        (multiremberT test? (cdr lat))) ))))

; Example of multiremberT
;
(display "------ multiremberT ------\n")
(multiremberT
    eq?-tuna
    '(shrimp salad tuna salad and tuna))
; ==> '(shrimp salad salad and)


; The multiremember&co uses a collector
;
(define multiremember&co
    (lambda (a lat col) 
        ))