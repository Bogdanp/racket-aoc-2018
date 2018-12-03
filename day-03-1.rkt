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
  (for/fold ([fabric (hash)])
            ([r rects])
    (for/fold ([fabric fabric])
              ([x (in-range (rect-x r) (+ (rect-x r) (rect-w r)))])
      (for/fold ([fabric fabric])
                ([y (in-range (rect-y r) (+ (rect-y r) (rect-h r)))])
        (define k (cons x y))
        (hash-set fabric k (add1 (hash-ref fabric k 0)))))))

(define (count-square-inches fabric)
  (for/fold ([overlapping 0])
            ([(p c) (in-hash fabric)]
             #:when (> c 1))
    (add1 overlapping)))

(count-square-inches
 (rects->fabric (map string->rect (file->lines "day-03-1.txt"))))
