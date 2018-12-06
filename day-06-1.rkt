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

(define (find-distances x1 x2 y1 y2)
  (for*/fold ([neighbors (hash)])
             ([x (in-range x1 x2)]
              [y (in-range y1 y2)])

    (define p (cons x y))
    (define distances-from-p
      (for/fold ([distances-from-p null]
                 #:result (sort distances-from-p (lambda (a b)
                                                   (< (cdr a)
                                                      (cdr b)))))
                ([p* positions])
        (cons (cons p* (manhattan-distance p p*)) distances-from-p)))

    (define closest-distance
      (match distances-from-p
        [(list (cons p _)) p]
        [(list (cons p d) (cons _ d) q ...) #f]
        [(list (cons p _) (cons q _) r ...) p]))

    (if closest-distance
        (hash-update neighbors closest-distance add1 0)
        neighbors)))

(define neighbors-1 (find-distances (- (car first-position) 50) (+ (car last-position) 50)
                                    (- (cdr first-position) 50) (+ (cdr last-position) 50)))
(define neighbors-2 (find-distances (- (car first-position) 51) (+ (car last-position) 51)
                                    (- (cdr first-position) 51) (+ (cdr last-position) 51)))

(displayln
 (for/fold ([unchanged (hash)]
            #:result (cdar (sort (hash->list unchanged) (lambda (a b)
                                                          (> (cdr a) (cdr b))))))
           ([p positions])
   (if (= (hash-ref neighbors-1 p 1)
          (hash-ref neighbors-2 p 2))
       (hash-set unchanged p (hash-ref neighbors-1 p))
       unchanged)))
