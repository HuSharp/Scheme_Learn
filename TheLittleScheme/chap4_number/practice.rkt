#lang racket
; Assume add1 is a primitive
;
(define add1
    (lambda (n)
        (+ n 1)))

; Example of add1
;
(add1 67)       ; 68

; Assume sub1 is a primitive
;
(define sub1
    (lambda (n)
        (- n 1)))

; Example of sub1
;
(sub1 5)        ; 4

(define olength
    (lambda (lat)
        (cond 
            ((null? lat) 0)
            (else (add1 (length (cdr lat))) ))))

; Examples of length
;
(display "------ olength ------\n")
(olength '(hotdogs with mustard sauerkraut and pickles))     ; 6
(olength '(ham and cheese on rye))                           ; 5


; The pick function returns the n-th element in a lat
;
(define pick
    (lambda (n lat)
        (cond 
            ((zero? (sub1 n)) (car lat))
            (else (pick (sub1 n) (cdr lat))))))

; Example of pick
;
(display "------ pick ------\n")
(pick 4 '(lasagna spaghetti ravioli macaroni meatball))     ; 'macaroni

; The rempick function removes the n-th element and returns the new lat
;
(define rempick
    (lambda (n lat)
        (cond 
            ((zero? (sub1 n)) (cdr lat))    ; 为 1 直接 return
            (else (cons (car lat)
                        (rempick (sub1 n) (cdr lat))) ))))

; Example of rempick
;
(display "------ rempick ------\n")
(rempick 3 '(hotdogs with hot mustard))     ; '(hotdogs with mustard)

; The no-nums function returns a new lat with all numbers removed
;
(define no-nums
    (lambda (lat) 
        (cond 
            ((null? lat) '())
            ((number? (car lat)) (no-nums (cdr lat))) 
            (else (cons (car lat) 
                        (no-nums (cdr lat)) ) ) )))

; Example of no-nums
;
(display "------ no-nums ------\n")
(no-nums '(5 pears 6 prunes 9 dates))       ; '(pears prunes dates)

; The all-nums does the opposite of no-nums - returns a new lat with
; only numbers
;
(define all-nums
    (lambda (lat) 
        (cond 
            ((null? lat) '())
            ((number? (car lat)) (cons (car lat) 
                                        (all-nums (cdr lat))))
            (else (all-nums (cdr lat))) )))

; Example of all-nums
;
(display "------ all-nums ------\n")
(all-nums '(5 pears 6 prunes 9 dates))       ; '(5 6 9)


; The eqan? function determines whether two arguments are te same
; It uses eq? for atoms and = for numbers
;
(define eqan?
    (lambda (m n) 
        (cond 
            ((and (number? m) (number? n)) (= m n))
            ((or (number? m) (number? n)) #f)
            (else (eq? m n)))))

; Examples of eqan?
;
(display "------ eqan ------\n")
(eqan? 3 3)     ; #t
(eqan? 3 4)     ; #f
(eqan? 'a 'a)   ; #t
(eqan? 'a 'b)   ; #f

; The occur function counts the number of times an atom appears
; in a list
;
(define occur
    (lambda (a lat)
        (cond 
            ((null? lat) 0)
            ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
            (else (occur a (cdr lat)) ))))

; Example of occur
;
(display "------ occur ------\n")
(occur 'x '(a b x x c d x))     ; 3
(occur 'x '())                  ; 0

; The one? function is true when n=1
;
(define one?
    (lambda (a) 
        (= a 1))) 

; Example of one?
;
(display "------ one ------\n")
(one? 5)        ; #f
(one? 1)        ; #t

; We can rewrite rempick using one?
; The rempick function removes the n-th element and returns the new lat
;
(define rempick2
    (lambda (n lat)
        (cond 
            ((one? n) (cdr lat))    ; 为 1 直接 return
            (else (cons (car lat)
                        (rempick2 (sub1 n) (cdr lat))) ))))

; Example of rempick
;
(display "------ rewite rempick ------\n")
(rempick2 3 '(hotdogs with hot mustard))     ; '(hotdogs with mustard)

