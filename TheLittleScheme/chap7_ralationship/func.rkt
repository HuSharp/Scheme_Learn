#lang racket
; Helper functions for working with pairs
;
(define first
    (lambda (p) 
        (car p)))

(define second
    (lambda (p) 
        (car (cdr p))))

(define build
    (lambda (s1 s2) 
        (cons s1 (cons s2 '()))))

(define third
    (lambda (p) 
        (car (cdr (cdr p)))))

; Example of a not-relations
;
'(apples peaches pumpkins pie)
'((apples peaches) (pumpkin pie) (apples peaches))

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
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))

; Examples of set?
;
(display "------ set ------\n")
(set? '(apples peaches pears plums))            ; #t
(set? '(apple peaches apple plum))              ; #f
(set? '(apple 3 pear 4 9 apple 3 4))            ; #f

; Examples of relations
;
(display "------ Examples of relations ------\n")
'((apples peaches) (pumpkin pie))
'((4 3) (4 2) (7 6) (6 2) (3 4))

; It uses firsts function from Chapter 3 (03-cons-the-magnificent.ss)
(define firsts
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (first (car l))
                        (firsts (cdr l)))))))

; The fun? function determines if rel is a function
;
(define fun?
    (lambda (rel) 
        (set? (firsts rel))))

; Examples of fun?
;
(display "------ fun? ------\n")
(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))     ; #f
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))     ; #t
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))     ; #f


; The revrel function reverses a relation
;
(define revrel
    (lambda (rel) 
        (cond 
            ((null? rel) '())
            (else (cons (build (second (car rel)) 
                            (first (car rel)))
                        (revrel (cdr rel)))))))

; Test of simplified revrel
; 
(revrel '((8 a) (pumpkin pie) (got sick)))
; ==> '((a 8) (pie pumpkin) (sick got))

; Let's simplify revrel by using inventing revpair that reverses a pair
;
(define revpair
    (lambda (pair) 
        (build (second pair) (first pair))))
; Simplified revrel
;
(define revrel_simplify
    (lambda (rel)
        (cond 
            ((null? rel) '())
            ((cons (revpair (car rel))
                    (revrel_simplify (cdr rel))) ))))
; Test of simplified revrel
; 
(revrel '((8 a) (pumpkin pie) (got sick)))
; ==> '((a 8) (pie pumpkin) (sick got))

; It uses seconds helper function
;
(define seconds
    (lambda (lat) 
        (cond
            ((null? lat) '()) 
            (else (cons (second (car lat))
            (seconds (cdr lat)))))))
; The fullfun? function determines if the given function is full
;
(define fullfun?
    (lambda (fun)
        (set? (seconds fun) )))

; Examples of fullfun?
;
(display "------ fullfun? ------\n")
(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))     ; #f
(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))     ; #t
(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))                    ; #f

; one-to-one? is the same fullfun?
;
(define one-to-one?
    (lambda (fun)
        (fun? (revrel fun))))

(display "------ one-to-one? ------\n")
(one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4)))     ; #f
(one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4)))     ; #t
(one-to-one? '((grape raisin)
                (plum prune)
                (stewed prune)))                    ; #f

(one-to-one? '((chocolate chip) (doughy cookie)))
; ==> #t and you deserve one now!