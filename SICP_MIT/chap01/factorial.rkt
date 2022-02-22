#lang racket
(provide (all-defined-out))
(require racket/trace)
;iter version

(define factorial
    (lambda (n) 
        (fact-iter 1 1 n)))

(define fact-iter
    (lambda (product cnt max-cnt) 
        (cond 
            ((> cnt max-cnt) product)
            (else (fact-iter (* cnt product)
                        (+ cnt 1) max-cnt) ))))
; (trace fact-iter)

; (factorial 5)
; (factorial 6)

