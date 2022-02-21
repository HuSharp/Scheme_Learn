#lang racket
(define new-if
    (lambda (predicate then-clause else-clause) 
        (cond 
            (predicate then-clause)
            (else else-clause))))

; 我们会发现两个语句都会被求值。
(new-if #t (display "good") (display "bad"))