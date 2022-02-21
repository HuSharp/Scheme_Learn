#lang racket
(define a-plus-abs-b
    (lambda (a b) 
        ((if (> b 0)
            +
            -) a b)))

(a-plus-abs-b 2 (- 2))
;Value: 4
(a-plus-abs-b 2 2)
;Value: 4