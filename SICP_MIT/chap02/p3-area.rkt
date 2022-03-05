#lang racket

(define (perimeter rec)
    (let ((len (length-of-rectangle rec))
            (wid (width-of-rectangle rec)))
        (* (+ len wid) 2.0)))

(define (area rec)
    (let ((len (length-of-rectangle rec))
            (wid (width-of-rectangle rec)))
        (* len wid)))  

;---------------- way 1 采用两对线段进行展示 -------------------
(display "\n-------------- way 1 采用两对线段进行展示  --------------")
; y
;
; 5
;    (1,4)     l1   (4,4)
; 4    +--------------+
;      |              |
; 3 w1 |              | w2
;      |              |
; 2    +--------------+
;    (1,2)     l2    (4,2)
; 1
;
; 0    1    2    3    4    5    x
; 其中 l1 的起点为 (1,4) ，终点为 (4,4) ，而 l2 的起点为 (1,2) ，终点为 (4,2) ； 
; w1 的起点为 (1,2) ，终点为 (1,4) ； w2 的起点为 (4,2) ，终点为 (4,4) 。
; construct
(define (make-rectangle l1 l2 w1 w2)
    (cons (cons l1 l2)
            (cons w1 w2)))
; selector
(define (length-1-rectangle r)
    (car (car r)))
(define (length-2-rectangle r)
    (cdr (car r)))
(define (width-1-rectangle r)
    (car (cdr r)))
(define (width-2-rectangle r)
    (cdr (cdr r)))

(define (print-rectangle r)
    (let ((l1 (length-1-rectangle r))
            (l2 (length-2-rectangle r))
            (w1 (width-1-rectangle r))
            (w2 (width-2-rectangle r)))

        (newline)
        (display "Length 1:")
        (print-point (start-segment l1))
        (print-point (end-segment l1))

        (newline)
        (display "Length 2:")
        (print-point (start-segment l2))
        (print-point (end-segment l2))

        (newline)
        (display "Width 1:")
        (print-point (start-segment w1))
        (print-point (end-segment w1))

        (newline)
        (display "Width 2:")
        (print-point (start-segment w2))
        (print-point (end-segment w2))))

(require "p2-segment.rkt")
(define length-1 (make-segment (make-point 1 4)
                                (make-point 4 4)))
(define length-2 (make-segment (make-point 1 2)
                                (make-point 4 2)))
(define width-1 (make-segment (make-point 1 2)
                                (make-point 1 4)))
(define width-2 (make-segment (make-point 4 2)
                                (make-point 4 4)))
(define rectangle (make-rectangle length-1 length-2 width-1 width-2))
(print-rectangle rectangle)


(define (length-of-rectangle rec)
    (let ((length (length-1-rectangle rec)))
        (let ((start (start-segment length))
                (end (end-segment length)))
            (- (x-point end) 
                (x-point start)))))
(define (width-of-rectangle rec)
    (let ((width (width-1-rectangle rec)))
        (let ((start (start-segment width))
                (end (end-segment width)))
            (- (y-point end) 
                (y-point start)))))
(display "\n-------- width & length -------\n")
(width-of-rectangle rectangle)
(length-of-rectangle rectangle)

(display "\n-------- perimeter & area -------\n")
(perimeter rectangle)
(area rectangle)


;---------------- way 2 采用两条线段进行展示 -------------------
(display "-------------- way 2 采用两条线段进行展示 --------------")
; y
;
; 5
;    (1,4)
; 4    +
;      |
; 3    | width
;      |
; 2    +--------------+
;    (1,2)   length  (4,2)
; 1
;
; 0    1    2    3    4    5    x
; 其中 length 的起点为 (1,2) ，终点为 (4,2) ， width 的起点为 (1,2) ，终点为 (1,4) 。
; construct
(define (make-rectangle-2 len wid)
    (cons len wid))
; selector
(define (length-rectangle rec)
    (car rec))

(define (width-rectangle rec)
    (cdr rec))


(define l (make-segment (make-point 1 2)
                        (make-point 4 2)))
(define w (make-segment (make-point 1 2)
                        (make-point 1 4)))
(define rectangle-2 (make-rectangle-2 l w))

(define (length-of-rectangle-2 rec)
    (let ((length (length-rectangle rec)))
        (let ((start (start-segment length))
                (end (end-segment length)))
            (- (x-point end) 
                (x-point start)))))
(define (width-of-rectangle-2 rec)
    (let ((width (width-rectangle rec)))
        (let ((start (start-segment width))
                (end (end-segment width)))
            (- (y-point end) 
                (y-point start)))))

(display "\n-------- width & length -------\n")
(width-of-rectangle-2 rectangle-2)
(length-of-rectangle-2 rectangle-2)

; 此处并不需要改名字，只是为了展示方便
(define (perimeter-2 rec)
    (let ((len (length-of-rectangle-2 rec))
            (wid (width-of-rectangle-2 rec)))
        (* (+ len wid) 2.0)))

(define (area-2 rec)
    (let ((len (length-of-rectangle-2 rec))
            (wid (width-of-rectangle-2 rec)))
        (* len wid)))  
(display "\n-------- perimeter & area -------\n")
(perimeter-2 rectangle-2)
(area-2 rectangle-2)