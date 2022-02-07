#lang racket
(define atom?
    (lambda (x) 
        (and (not (pair? x)) (not (null? x)))))

; Examples of atom?
;
(atom? 'Harry)                          ; true
(atom? '(Harry had a heap of apples))   ; false

; Examples of atom?, car and cdr
;
(atom? (car '(Harry had a heap of apples)))         ; true
(atom? (cdr '(Harry had a heap of apples)))         ; false
(atom? (cdr '(Harry)))                              ; false
(atom? (car (cdr '(swing low sweet cherry oat))))   ; true
(atom? (car (cdr '(swing (low sweet) cherry oat)))) ; false