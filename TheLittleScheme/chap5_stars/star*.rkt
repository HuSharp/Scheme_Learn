#lang racket
; The atom? primitive
;
(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

; The add1 primitive
;
(define add1
    (lambda (n) (+ n 1)))

; The rember* function removes all matching atoms from an s-expression
;
(define rember*
    (lambda (a lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat))
                (cond 
                    ((eq? (car lat) a) (rember* a (cdr lat)))
                    (else (cons (car lat)
                                (rember* a (cdr lat))) )))
            (else (cons (rember* a (car lat))           ; (car lat) 可以不为原子
                        (rember* a (cdr lat)) ) ) )))

; Examples of rember*
;
(display "------ rember* ------\n")
(rember*
    'cup
    '((coffee) cup ((tea) cup) (and (hick)) cup))
;==> '((coffee) ((tea)) (and (hick)))

(rember*
    'sauce
    '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
;==> '(((tomato)) ((bean)) (and ((flying))))


(define insertR*
    (lambda (new old lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat))
                (cond 
                    ((eq? (car lat) old) (cons old
                                            (cons new 
                                                (insertR* new old (cdr lat)))))
                    (else (cons (car lat)
                                (insertR* new old (cdr lat)))) ))
            (else (cons (insertR* new old (car lat))
                        (insertR* new old (cdr lat))) ))))

; Example of insertR*
;
(display "------ insertR* ------\n")
(insertR*
    'roast
    'chuck
    '((how much (wood)) could ((a (wood) chuck)) (((chuck)))
        (if (a) ((wood chuck))) could chuck wood))
; ==> ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast)))
;      (if (a) ((wood chuck roast))) could chuck roast wood)

            
; The occur* function counts the number of occurances of an atom in l
;
(define occur*
    (lambda (a lat) 
        (cond 
            ((null? lat) 0)
            ((atom? (car lat))
                (cond 
                    ((eq? (car lat) a) (add1 (occur* a (cdr lat))))
                    (else (occur* a (cdr lat)) )))                   
            (else (+ (occur* a (car lat)) (occur* a (cdr lat)) ) ) ) ) )

; Example of occur*
;
(display "------ occur* ------\n")
(occur*
    'banana
    '((banana)
    (split ((((banana ice)))
            (cream (banana))
            sherbet))
    (banana)
    (bread)
    (banana brandy)))
;==> 5


; The subst* function substitutes all olds for news in l
;
(define subst*
    (lambda (new old lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat))
                (cond 
                    ((eq? (car lat) old) 
                        (cons new 
                            (subst* new old (cdr lat)) ))
                    (else (cons (car lat)
                            (subst* new old (cdr lat))))))
            (else (cons (subst* new old (car lat)) 
                        (subst* new old (cdr lat)) )))))
; Example of subst*
;
(display "------ subst* ------\n")
(subst*
    'orange
    'banana
    '((banana)
    (split ((((banana ice)))
            (cream (banana))
            sherbet))
    (banana)
    (bread)
    (banana brandy)))
;==> '((orange)
;      (split ((((orange ice)))
;              (cream (orange))
;              sherbet))
;      (orange)
;      (bread)
;      (orange brandy))


; The member* function determines if element is in a list l of s-exps
;
(define member*
    (lambda (a lat)
        (cond 
            ((null? lat) #f)
            ((atom? (car lat)) 
                (or (eq? (car lat) a) 
                    (member* a (cdr lat))) )
            (else (or (member* a (car lat))
                    (member* a (cdr lat))) ))))

; Example of member*
;
(display "------ member* ------\n")
(member*
    'chips
    '((potato) (chips ((with) fish) (chips))))    ; #t


; The leftmost function finds the leftmost atom in a non-empty list
; of S-expressions that doesn't contain the empty list
;
(define leftmost
    (lambda (lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat)) (car lat))
            (else (leftmost (car lat))))))

; Examples of leftmost
;
(display "------ leftmost* ------\n")
(leftmost '((potato) (chips ((with) fish) (chips))))    ; 'potato
(leftmost '(((hot) (tuna (and))) cheese))               ; 'hot

; Examples of not-applicable leftmost
;
(leftmost '(((() four)) 17 (seventeen))) ; leftmost s-expression is empty
(leftmost '())                           ; empty list

; The eqlist? function determines if two lists are equal
;
(define eqlist?
    (lambda (l1 l2)
        (cond
        ; case 1: l1 is empty, l2 is empty, atom, list
            ((and (null? l1) (null? l2)) #t)
            ((and (null? l1) (atom? (car l2))) #f)
            ((null? l1) #f)     ; l2 is list
        ; case 2: l1 is atom, l2 is empty, atom, list
            ((and (atom? (car l1)) (null? l2)) #f)
            ((and (atom? (car l1)) (atom? (car l2)))
                (and (eq? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2))))
            ((atom? (car l1)) #f)   ; l2 is list
        ; case 3: l1 is a list, l2 is empty, atom, list
            ((null? l2) #f)
            ((atom? (car l2)) #f)
            (else
                (and (eqlist? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2)))))))


; Example of eqlist?
;
(display "------ eqlist? ------\n")
(eqlist?
    '(strawberry ice cream)
    '(strawberry ice cream))                  ; #t

(eqlist?
    '(strawberry ice cream)
    '(strawberry cream ice))                  ; #f

(eqlist?
    '(banan ((split)))
    '((banana) split))                        ; #f

(eqlist?
    '(beef ((sausage)) (and (soda)))
    '(beef ((salami)) (and (soda))))          ; #f

(eqlist?
    '(beef ((sausage)) (and (soda)))
    '(beef ((sausage)) (and (soda))))         ; #t

; eqlist? rewritten
;
(define eqlist2?
    (lambda (l1 l2)
        (cond
        ; case 1: l1 is empty, l2 is empty, atom, list
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
        ; case 2: l1 is atom, l2 is empty, atom, list
            ((and (atom? (car l1)) (atom? (car l2)))
            (and (eq? (car l1) (car l2))
                (eqlist2? (cdr l1) (cdr l2))))
            ((or (atom? (car l1)) (atom? (car l2))) #f)
        ; case 3: l1 is a list, l2 is empty, atom, list
            (else
                (and (eqlist2? (car l1) (car l2))
                    (eqlist2? (cdr l1) (cdr l2)))))))

; Tests of eqlist2?
;
(display "------ eqlist2? ------\n")
(eqlist2?
    '(strawberry ice cream)
    '(strawberry ice cream))                  ; #t

(eqlist2?
    '(strawberry ice cream)
    '(strawberry cream ice))                  ; #f

(eqlist2?
    '(banan ((split)))
    '((banana) split))                        ; #f

(eqlist2?
    '(beef ((sausage)) (and (soda)))
    '(beef ((salami)) (and (soda))))          ; #f

(eqlist2?
    '(beef ((sausage)) (and (soda)))
    '(beef ((sausage)) (and (soda))))         ; #t


; The equal? function determines if two s-expressions are equal
;
(define equal??
    (lambda (s1 s2)
        (cond 
            ((and (atom? s1) (atom? s2)) 
                (eq? s1 s2))
            ((atom? s1) #f)
            ((atom? s2) #f)
            (else (eqlist? s1 s2)))))

; Examples of equal??
;
(display "------ equal?? ------\n")
(equal?? 'a 'a)                              ; #t
(equal?? 'a 'b)                              ; #f
(equal?? '(a) 'a)                            ; #f
(equal?? '(a) '(a))                          ; #t
(equal?? '(a) '(b))                          ; #f
(equal?? '(a) '())                           ; #f
(equal?? '() '(a))                           ; #f
(equal?? '(a b c) '(a b c))                  ; #t
(equal?? '(a (b c)) '(a (b c)))              ; #t
(equal?? '(a ()) '(a ()))                    ; #t

; equal?? simplified
;
(define equal2??
    (lambda (s1 s2)
        (cond 
            ((and (atom? s1) (atom? s2)) 
                (eq? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))

; Tests of equal2??
;
(display "------ equal2?? ------\n")
(equal2?? 'a 'a)                              ; #t
(equal2?? 'a 'b)                              ; #f
(equal2?? '(a) 'a)                            ; #f
(equal2?? '(a) '(a))                          ; #t
(equal2?? '(a) '(b))                          ; #f
(equal2?? '(a) '())                           ; #f
(equal2?? '() '(a))                           ; #f
(equal2?? '(a b c) '(a b c))                  ; #t
(equal2?? '(a (b c)) '(a (b c)))              ; #t
(equal2?? '(a ()) '(a ()))                    ; #t

; eqlist? rewritten using equal2??
;
(define eqlist3?
    (lambda (l1 l2) 
        (cond 
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and (equal2?? (car l1) (car l2)) 
                        (eqlist3? (cdr l1) (cdr l2)))))))

; Tests of eqlist3?
;
(display "------ eqlist3? ------\n")
(eqlist3?
    '(strawberry ice cream)
    '(strawberry ice cream))                  ; #t

(eqlist3?
    '(strawberry ice cream)
    '(strawberry cream ice))                  ; #f

(eqlist3?
    '(banan ((split)))
    '((banana) split))                        ; #f

(eqlist3?
    '(beef ((sausage)) (and (soda)))
    '(beef ((salami)) (and (soda))))          ; #f

(eqlist3?
    '(beef ((sausage)) (and (soda)))
    '(beef ((sausage)) (and (soda))))         ; #t



(define rember
    (lambda (a lat) 
        (cond 
            ((null? lat) '())
            ((eq? (car lat) a) 
                (rember a (cdr lat)))
            (else (cons (car lat)
                    (rember a (cdr lat)))))))

; Examples of rember function
;
(display "------ rember ------\n")
(rember 'mint '(lamb chops and mint flavored mint jelly)) ; '(lamb chops and flavored mint jelly)
(rember 'toast '(bacon lettuce and tomato))               ; '(bacon lettuce and tomato)
(rember 'cup '(coffee cup tea cup and hick cup))          ; '(coffee tea cup and hick cup)

; rember, it now also works on s-expressions, not just atoms
;
(define rember-s
    (lambda (s lat)
        (cond 
            ((null? lat) '())
            ((atom? (car lat)) 
                (cond 
                    ((equal2?? (car lat) s) (rember-s s (cdr lat)))
                    (else (cons (car lat)
                            (rember-s s (cdr lat))) )))
            (else (cond 
                ((equal2?? (car lat) s) (cdr lat))
                (else (cons (car lat) (rember-s s (cdr lat))) ))) )))

; Example of rember
;
(display "------ rember-s ------\n")
(rember-s
    '(foo (bar (baz)))
    '(apples (foo (bar (baz))) oranges))
;==> '(apples oranges)

; rember simplified-1, it now also works on s-expressions, not just atoms
;
(define rember-s2
    (lambda (s lat) 
        (cond 
            ((null? lat) '())
            (else (cond 
                ((equal2?? (car lat) s) (cdr lat))
                (else (cons (car lat) (rember-s2 s (cdr lat))) ))))))

; Example of rember-s2
;
(display "------ rember-s2 ------\n")
(rember-s2
    '(foo (bar (baz)))
    '(apples (foo (bar (baz))) oranges))
;==> '(apples oranges)

; rember simplified-2, it now also works on s-expressions, not just atoms
;
(define rember-s3
    (lambda (s lat) 
        (cond 
            ((null? lat) '())
            ((equal2?? (car lat) s) (cdr lat))
            (else (cons (car lat) (rember-s s (cdr lat))) ))))

; Example of rember-s2
;
(display "------ rember-s3 ------\n")
(rember-s3
    '(foo (bar (baz)))
    '(apples (foo (bar (baz))) oranges))
;==> '(apples oranges)