#lang racket
;pra1.1 填入就行

;pra1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7)))
;=> -37/150 

;pra1.3
(define bigger
    (lambda (x y) 
        (if (> x y)
            x
            y)))


