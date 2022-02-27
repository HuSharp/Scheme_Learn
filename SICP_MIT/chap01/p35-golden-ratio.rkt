#lang racket
(provide (all-defined-out))
(require "fixed-point.rkt")

(define golden-ratio
    (- (fixed-point 
        (lambda (x) (+ 1 (/ 1 x)))
        1.0) 1))

(display golden-ratio)
(newline)