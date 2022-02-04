#lang racket
(define x 20)
(let ((x 1)
        (y 2)
        (z 3))
    (list x y z))

(let ((x 1) (y x))
    (+ x y))

(let* ((x 1))
    (let ((y x))
        (+ x y)))

(let ((are (lambda (x y) (+ x y)))) (are 2 3))