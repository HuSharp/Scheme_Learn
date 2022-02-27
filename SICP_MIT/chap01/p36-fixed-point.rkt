#lang racket
(define average
    (lambda (x y) 
        (/ (+ x y) 2)))

(define (fixed-point f first-guess)
    (define (close-enough? x y)
        (< (abs (- x y)) 0.00001))
    (define (try guess step)
        (display-info guess step)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                (begin 
                    (display-info next (+ 1 step)))
                (try next (+ 1 step)))))
    (try first-guess 1))

(define (display-info guess step)
    (display "Step: ")
    (display step)
    (display " ")
    
    (display "Guess: ")
    (display guess)
    (newline))

(define (average-damp f)
    (lambda (x)
        (average x (f x))))

(display "---------- before average-damp ------------ \n")
(fixed-point
    (lambda (x) (/ (log 1000) (log x)))
    2.0)
(display "---------- after average-damp ------------\n ")
(fixed-point
    (average-damp (lambda (x) (/ (log 1000) (log x))))
    2.0)
; 我们发现不带平均阻尼的计算使用了 35 步，另一方面，使用平均阻尼的计算只使用了 10 步