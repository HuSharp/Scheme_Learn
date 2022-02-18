#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

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


; The multirember&co uses a collector
;
(define multirember&co
    (lambda (a lat col) 
        (cond 
            ((null? lat) (col '() '()))
            ((eq? (car lat) a)
                (multirember&co a (cdr lat)
                    (lambda (newlat seen) 
                            (col newlat
                                (cons (car lat) seen)))))
            (else (multirember&co a (cdr lat)
                    (lambda (newlat seen) 
                            (col (cons (car lat)
                                    newlat) seen)))))))

(define a-friend
    (lambda (x y) 
        (null? y)))

; Examples of multirember&co with friendly function
;
(display "------ multirember&co ------\n")
(multirember&co
    'tuna
    '(strawberries tuna and swordfish)
    a-friend)
; ==> #f
(multirember&co
    'tuna
    '()
    a-friend)
; ==> #t
(multirember&co
    'tuna
    '(tuna)
    a-friend)
; ==> #f
(multirember&co
    'tuna
    '(the tuna)
    a-friend)
; ==> #f
(multirember&co
    'tuna
    '(the)
    a-friend)
; ==> #t



(define friend-latest
    (lambda (newlat seen) 
        (a-friend (cons 'and newlat)
                seen)))

; The multiinsertLR inserts to the left and to the right of elements
;
(define multiinsertLR
    (lambda (new oldL oldR lat) 
        (cond 
            ((null? lat) '())
            ((eq? (car lat) oldL)
                (cons new
                    (cons oldL
                        (multiinsertLR new oldL oldR (cdr lat)))))
            ((eq? (car lat) oldR)
                (cons oldR
                    (cons new
                        (multiinsertLR new oldL oldR (cdr lat)))))
            (else (cons (car lat)
                        (multiinsertLR new oldL oldR (cdr lat))) ))))

; Example of multiinsertLR
;
(display "------ multiinsertLR ------\n")
(multiinsertLR
    'x
    'a
    'b
    '(a o a o b o b b a b o))
; ==> '(x a o x a o b x o b x b x x a b x o)

; The multiinsertLR&co is to multiinsertLR what multirember is to
; multiremember&co
;
(define multiinsertLR&co
    (lambda (new oldL oldR lat col) 
        (cond 
            ((null? lat) (col '() 0 0))
            ((eq? (car lat) oldL)
                (multiinsertLR&co new oldL oldR
                    (cdr lat)
                    (lambda (newlat L R) 
                        (col (cons new 
                                (cons oldL newlat)) (+ 1 L) R))))
            ((eq? (car lat) oldR)
                (multiinsertLR&co new oldL oldR
                    (cdr lat)
                    (lambda (newlat L R) 
                        (col (cons oldR
                                (cons new newlat)) L (+ 1 R) ))))
            (else (multiinsertLR&co new oldL oldR
                    (cdr lat)
                    (lambda (newlat L R) 
                        (col (cons (car lat) newlat) L R)))))))
; Some collectors
;
(define col1
    (lambda (lat L R)
        lat))
(define col2
    (lambda (lat L R)
        L))
(define col3
    (lambda (lat L R)
        R))

; Examples of multiinsertLR&co
;
(multiinsertLR&co
    'salty
    'fish
    'chips
    '(chips and fish or fish and chips)
    col1)
; ==> '(chips salty and salty fish or salty fish and chips salty)
(multiinsertLR&co
    'salty
    'fish
    'chips
    '(chips and fish or fish and chips)
    col2)
; ==> 2
(multiinsertLR&co
    'salty
    'fish
    'chips
    '(chips and fish or fish and chips)
    col3)
; ==> 2


; The evens-only* function leaves all even numbers in an s-expression
; (removes odd numbers)
;
(define even-only*
    (lambda (lat) 
        (cond 
            ((null? lat) '())
            ((atom? (car lat))
                (cond 
                    ((even? (car lat)) 
                        (cons (car lat)
                            (even-only* (cdr lat))))
                    (else (even-only* (cdr lat)) )))
            (else (cons (even-only* (car lat))
                        (even-only* (cdr lat)))) )))


; Example of evens-only*
;
(display "------ even-only* ------\n")
(even-only*
    '((9 1 2 8) 3 10 ((9 9) 7 6) 2))  ; '((2 8) 10 (() 6) 2)

; Evens only function with a collector, collects evens, their product,
; and sum of odd numbers
;
(define even-only*&co
    (lambda (lat col) 
        (cond 
            ((null? lat) (col '() 1 0)) ; 偶数列表 偶数项的乘积  奇数项的和
            ((atom? (car lat))
                (cond 
                    ((even? (car lat)) 
                        (even-only*&co (cdr lat)
                            (lambda (newlat mul sum) 
                                (col (cons (car lat) newlat)
                                    (* (car lat) mul) sum))))
                    (else (even-only*&co (cdr lat)
                            (lambda (newlat mul sum) 
                                (col newlat
                                    mul (+ sum (car lat))))))))
            (else (even-only*&co (car lat)
                    (lambda (al ap as)      ;  处理 (car lat) 时得到的结果
                        (even-only*&co (cdr lat)
                                (lambda (dl dp ds)  ; 处理 (cdr lat) 时得到的结果
                                    (col (cons al dl)
                                        (* ap dp)
                                        (+ as ds)))))) ))))
(display "------ even-only*&co ------\n")
; evens-friend returns collected evens
;
(define evens-friend
    (lambda (e p s)
        e))

; Example of evens-friend used
;
(even-only*&co 
    '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
    evens-friend)
; ==> '((2 8) 10 (() 6) 2)

; evens-product-friend returns the product of evens
;
(define evens-product-friend
    (lambda (e p s)
        p))

; Example of evens-product-friend used
;
(even-only*&co 
    '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
    evens-product-friend)
; ==> 1920

; evens-sum-friend returns the sum of odds
;
(define evens-sum-friend
    (lambda (e p s)
        s))

; Example of evens-sum-friend used
;
(even-only*&co 
    '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
    evens-sum-friend)
; ==> 38

; the-last-friend returns sum, product and the list of evens consed together
;
(define the-last-friend
    (lambda (e p s)
        (cons s (cons p e))))

; Example of the-last-friend
;
(even-only*&co 
    '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
    the-last-friend)
; ==> '(38 1920 (2 8) 10 (() 6) 2)


;------------------------  explain collector ---------------------------
; https://luciaca.cn/posts/understanding-collector-in-scheme/
;
(define number-only*&co
    (lambda (lat col)
        (cond 
            ((null? lat) (col '() 0))
            ((atom? (car lat))
                (cond 
                    ((number? (car lat)) 
                        (number-only*&co (cdr lat)
                            (lambda (newlat sum) 
                                (col (cons (car lat) newlat)
                                    (+ sum (car lat))))))
                    (else (number-only*&co (cdr lat) col) )))
            ; else 后的这个 collector 需要使用 number-only*&co 来遍历 (cdr l)，挑出(cdr l)中的所有数字，并算出这些数字的和。
            (else (number-only*&co (car lat)
                ;「这里al, as 是 number-only*&co 处理 (car l) 时得到的结果，
                ; 而dl, ds 是 number-only*&co 处理 (cdr l) 时得到的结果。」
                        (lambda (alat asum) 
                            (number-only*&co (cdr lat)
                                (lambda (dlat dsum) 
                                    (col (cons alat dlat) 
                                        (+ asum dsum)))))) ))))

(define cal-number
    (lambda (newlat sum) 
        (cons sum (cons newlat '()))))

(display "------ cal-number ------\n")
; 输出结果的格式（sum newlist), list的第一个元素是求和的结果，第二个元素为所取到数字集。
;
(number-only*&co '(9 apples 2 oranges 10 peaches 11 pears) cal-number)
;Value : (32 (9 2 10 11))

(number-only*&co '(9 apples (2 oranges) (10) peaches ((11)) pears) cal-number)
;Value : (32 (9 (2) (10) ((11))))
