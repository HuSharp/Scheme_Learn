#lang racket
(define member?
    (lambda (a lat) 
        (cond 
            ((null? lat) #f)
            (else (or (eq? (car lat) a) (member? a (cdr lat))) ))))

; The set? function determines if a given lat is a set
;
(define set?
    (lambda (lat)
        (cond 
            ((null? lat) #t)
            ((member? (car lat) (cdr lat))  #f)
            (else (set? (cdr lat))))))

; Examples of set?
;
(display "------ set ------\n")
(set? '(apples peaches pears plums))            ; #t
(set? '(apple peaches apple plum))              ; #f
(set? '(apple 3 pear 4 9 apple 3 4))            ; #f


; The makeset funciton takes a lat and produces a set
;
(define makeset
    (lambda (lat)
        (cond 
            ((null? lat) '())
            ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
            (else (cons (car lat) 
                        (makeset (cdr lat)))))))

; Example of makeset
;
(display "------ makeset ------\n")
(makeset '(apple peach pear peach plum apple lemon peach))
; ==> '(pear plum apple lemon peach)

; makeset via multirember from Chapter 3 (03-cons-the-magnificent.ss)
; The multirember function removes all occurances of a from lat
;
(define multirember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            ((eq? a (car lat)) (multirember a (cdr lat)))
            (else (cons (car lat) 
                        (multirember a (cdr lat)))) )))


(define makeset_multirember
    (lambda (lat)
        (cond 
            ((null? lat) '())
            (else (cons (car lat) 
                        (makeset_multirember 
                            (multirember (car lat) (cdr lat)))) ))))

; Test makeset
;
(display "------ makeset_multirember ------\n")
(makeset_multirember '(apple peach pear peach plum apple lemon peach))
; ==> '(apple peach pear plum lemon)

(makeset_multirember '(apple 3 pear 4 9 apple 3 4))
; ==> '(apple 3 pear 4 9)

; The subset? function determines if set1 is a subset of set2
;
(define subset?
    (lambda (set1 set2) 
        (cond 
            ((null? set1) #t)
            (else 
                (cond 
                    ((member? (car set1) set2)
                        (subset? (cdr set1) set2))
                    (else #f)) ))))
; Examples of subset?
;
(display "------ subset? ------\n")
(subset? '(5 chicken wings)
        '(5 hamburgers 2 pieces fried chicken and light duckling wings))
; ==> #t

(subset? '(4 pounds of horseradish)
        '(four pounds of chicken and 5 ounces of horseradish))
; ==> #f

; A shorter version of subset?
;
(define subset_simplify?
    (lambda (set1 set2) 
        (cond 
            ((null? set1) #t)
            (else 
                (and (member? (car set1) set2) 
                    (subset_simplify? (cdr set1) set2) )))))

; Tests of the new subset?
;
(display "------ subset_simplify? ------\n")
(subset_simplify? '(5 chicken wings)
        '(5 hamburgers 2 pieces fried chicken and light duckling wings))
; ==> #t

(subset_simplify? '(4 pounds of horseradish)
        '(four pounds of chicken and 5 ounces of horseradish))
; ==> #f

; The eqset? function determines if two sets are equal
;
(define eqset?
    (lambda (set1 set2) 
        (and (subset? set1 set2) 
            (subset? set2 set1))))

; Examples of eqset?
;
(display "------ eqset? ------\n")
(eqset? '(a b c) '(c b a))          ; #t
(eqset? '() '())                    ; #t
(eqset? '(a b c) '(a b))            ; #f


; The intersect? function finds if two sets intersect
;
(define intersect?
    (lambda (set1 set2) 
        (cond 
            ((null? set1) #f)
            (else 
                (or (member? (car set1) set2)
                    (intersect? (cdr set1) set2))))))

; Tests of intersect?
;
(display "------ intersect? ------\n")
(intersect?
    '(stewed tomatoes and macaroni)
    '(macaroni and cheese))
; ==> #t

(intersect?
    '(a b c)
    '(d e f))
; ==> #f

; The intersect function finds the intersect between two sets
;
(define intersect
    (lambda (set1 set2) 
        (cond 
            ((null? set1) '())
            ((member? (car set1) set2)
                (cons (car set1)
                    (intersect (cdr set1) set2)))
            (else (intersect (cdr set1) set2) ))))

; Example of intersect
;
(display "------ intersect ------\n")
(intersect
    '(stewed tomatoes and macaroni)
    '(macaroni and cheese))
; ==> '(and macaroni)

; The union function finds union of two sets
;
(define union
    (lambda (set1 set2) 
        (cond 
            ((null? set1) set2)
            ((member? (car set1) set2)
                (union (cdr set1) set2))
            (else (cons (car set1)
                        (union (cdr set1) set2)) ))))

; Example of union
;
(display "------ union ------\n")
(union
    '(stewed tomatoes and macaroni casserole)
    '(macaroni and cheese))
; ==> '(stewed tomatoes casserole macaroni and cheese)


; The xxx function is the set difference function
; return the set which in set1 but notesin set2
(define xxx
    (lambda (set1 set2) 
        (cond 
            ((null? set1) '())
            ((member? (car set1) set2)
                (xxx (cdr set1) set2))
            (else (cons (car set1)
                        (xxx (cdr set1) set2)) ))))

; Example of set difference
;
(display "------ xxx ------\n")
(xxx '(a b c) '(a b d e f))     ; '(c)
(xxx '(a b) '(a b d e f))     ; '()
(xxx '(a c b m) '(a b d e f))     ; '(c m)


; The intersectall function finds intersect between multitude of sets
;
(define intersectall
    (lambda (l-set) 
        (cond 
            ((null? (cdr l-set)) (car l-set))
            (else (intersect (car l-set)
                    (intersectall (cdr l-set)))))))
; Examples of intersectall
;
(display "------ intersectall ------\n")
(intersectall '((a b c) (c a d e) (e f g h a b)))       ; '(a)
(intersectall
    '((6 pears and)
    (3 peaches and 6 peppers)
    (8 pears and 6 plums)
    (and 6 prunes with some apples)))                   ; '(6 and)


(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
; The a-pair? function determines if it's a pair
;
(define a-pair?
    (lambda (x) 
        (cond 
            ((atom? x) #f)
            ((null? x) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else  #f))))

; Examples of pairs
;
(display "------ pairs ------\n")
(a-pair? '(pear pear))          ; #t
(a-pair? '(3 7))                ; #t
(a-pair? '((2) (pair)))         ; #t
(a-pair? '(full (house)))       ; #t

; Examples of not-pairs
(a-pair? '())                   ; #f
(a-pair? '(a b c))              ; #f

