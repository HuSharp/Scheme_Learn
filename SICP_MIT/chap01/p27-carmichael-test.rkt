#lang racket
(require "fermat_prime.rkt")

(define carmichael-test
    (lambda (n) 
        (test-iter 1 n)))

(define test-iter
    (lambda (a n) 
        (cond 
            ((= a n) #t)
            ((congruent? a n) 
                (test-iter (+ a 1) n))
            (else #f))))

(define congruent?
    (lambda (a n) 
        (= (expmod a n n) a)))

(display "------------ carmichael-test -------------\n")
(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)