#lang racket 

(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

; f(x) = 0
; 但是必须要求是正相关的 即 f(-)=-
(define (search f neg-point pos-point)
    (let ((mid-point (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            mid-point
            (let ((test-val (f mid-point)))
                (cond 
                    ((positive? test-val) 
                        (search f neg-point mid-point))
                    ((negative? test-val)
                        (search f mid-point pos-point))
                    (else mid-point ))))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

; 因为上面的 search 函数必须要求是正相关的 即 f(-)=-
; 因此，很难直接被调用
(define (half-interval-method f a b)
    (let ((a-val (f a))
            (b-val (f b)))
        (cond 
            ((and (negative? a-val) (positive? b-val) (search f a b)) )
            ((and (negative? b-val) (positive? a-val) (search f b a)) )
            (else (error "Values are not") ))))

(half-interval-method sin 2.0 4.0)

; x^3-2x-3=0
(half-interval-method
    (lambda (x) (- (* x x x) (* x 2) 3))
    1.0 3.0)