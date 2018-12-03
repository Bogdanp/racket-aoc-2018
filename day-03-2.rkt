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

(define (rect-claimed? r fabric)
  (let loop ([x (rect-x r)]
             [y (rect-y r)])
    (cond
      [(>= y (+ (rect-y r) (rect-h r))) #f]
      [(>= x (+ (rect-x r) (rect-w r))) (loop (rect-x r) (add1 y))]
      [(> (hash-ref fabric (cons x y)) 1) #t]
      [else (loop (add1 x) y)])))

(define (rects->fabric rects)
  (for/fold ([fabric (hash)])
            ([r rects])
    (for/fold ([fabric fabric])
              ([x (in-range (rect-x r) (+ (rect-x r) (rect-w r)))])
      (for/fold ([fabric fabric])
                ([y (in-range (rect-y r) (+ (rect-y r) (rect-h r)))])
        (define k (cons x y))
        (hash-set fabric k (add1 (hash-ref fabric k 0)))))))

(define (find-sole-claim rects)
  (define fabric (rects->fabric rects))
  (for/first ([r rects] #:when (not (rect-claimed? r fabric)))
    (rect-id r)))

(find-sole-claim (map string->rect (file->lines "day-03-1.txt")))
