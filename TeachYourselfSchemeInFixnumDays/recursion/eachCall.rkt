#lang racket
(
    define (type0 ? x) (
        if (= x 0) #t
        (type2 ? (- x 1))
    )
) (
    define (type1 ? x) (
        if (= x 0) #f
        (type0 ? (- x 1))
    )
) (
    define (type2 ? x) (
        if (= x 0) #f
        (type1 ? (- x 1))
    )
)

(for-each
 (lambda (x) (display x)(newline))
 (map
  (lambda (x)
   (cons
    x
    (map (lambda (f) (f x)) (list type0? type1? type2?))
   )
  )
  (range 20)
 )
)