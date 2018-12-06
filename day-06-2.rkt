#lang racket

(define (manhattan-distance va vb)
  (+ (abs (- (car va) (car vb)))
     (abs (- (cdr va) (cdr vb)))))

(define positions
  (call-with-input-file "day-06-1.txt"
    (lambda (in)
      (let loop ([positions null]
                 [line (read-line in)])
        (cond
          [(eof-object? line)
           (sort positions (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b))))))]

          [else
           (match-define (list x y)
             (map string->number (string-split line ", ")))
           (loop (cons (cons x y) positions) (read-line in))])))))

(define first-position (first positions))
(define last-position (last positions))

(define (find-region x1 x2 y1 y2)
  (for*/sum ([x (in-range x1 x2)]
             [y (in-range y1 y2)])

    (define p (cons x y))
    (define sum-of-distances
      (for/sum ([p* positions])
        (manhattan-distance p p*)))

    (if (< sum-of-distances 10000)
        1
        0)))

(define buffer (/ 10000 (length positions)))
(displayln (find-region (- (car first-position) buffer) (+ (car last-position) buffer)
                        (- (cdr first-position) buffer) (+ (cdr last-position) buffer)))
