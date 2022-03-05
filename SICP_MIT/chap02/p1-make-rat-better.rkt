#lang racket
(require "gcd.rkt")
; 当有理数为负数时，只要分子为负数
(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (if (< d 0)
            (cons (- (/ n g)) (- (/ d g)))
            (cons (/ n g) (/ d g)) )))

(make-rat 1 2)
(make-rat -1 2)
(make-rat 1 -2)
(make-rat -1 -2)

(make-rat 4 6)
(make-rat -4 6)
(make-rat 4 -6)
(make-rat -4 -6)