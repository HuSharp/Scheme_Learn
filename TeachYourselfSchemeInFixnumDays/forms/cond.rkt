#lang racket
(define c #\c)
(cond ((char>? c #\c) -1)
    ((char=? c #\c) (display "0"))
    (else 1))