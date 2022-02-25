#lang racket
(define cnt-change
    (lambda (amount) 
        (cc amount 5)))

(define cc
    (lambda (amount kinds-of-coins) 
        (cond 
            ((= 0 amount) 1)
            ((or (< amount 0) (= kinds-of-coins 0)) 0)
            (else (+ (cc amount (- kinds-of-coins 1))
                    (cc (- amount (first-denomination kinds-of-coins))
                        kinds-of-coins))) )))

(define first-denomination
    (lambda (kinds-of-coins) 
        (cond 
            ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 25)
            ((= kinds-of-coins 5) 50) )))

(cnt-change 100)