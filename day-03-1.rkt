#lang racket

(struct rect (id x y w h)
  #:transparent)

(define LINE-RE #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)")
(define (string->rect s)
  (match-define (list _ id x y w h) (regexp-match LINE-RE s))
  (rect (string->number id)
        (string->number x)
        (string->number y)
        (string->number w)
        (string->number h)))

(define (rects->fabric rects)
  (for*/fold ([fabric (hash)])
             ([r rects]
              [x (in-range (rect-x r) (+ (rect-x r) (rect-w r)))]
              [y (in-range (rect-y r) (+ (rect-y r) (rect-h r)))])
    (hash-update fabric (cons x y) add1  0)))

(define (count-square-inches fabric)
  (for/fold ([overlapping 0])
            ([(p c) (in-hash fabric)] #:when (> c 1))
    (add1 overlapping)))

(count-square-inches
 (rects->fabric (map string->rect (file->lines "day-03-1.txt"))))
