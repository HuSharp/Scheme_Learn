#lang racket
(define p 80)
(if (> p 90)
    (begin 
        (display "then happy")
        (display "happy2")
    )
    (begin 
        (display "else happy")
        (display "happy2")
    ))