#lang racket

(define words (file->lines "day-02-2.txt"))

(define (delta a b)
  (for/fold ([s ""])
            ([ac (in-string a)]
             [bc (in-string b)])
    (if (equal? ac bc)
        (~a s ac)
        s)))

(for/fold ([best ""])
          ([i (in-range 0 (length words))]
           [word-a (in-list words)])
  (for/fold ([best best])
            ([j (in-range 0 (length words))]
             [word-b (in-list words)])
    (define d (delta word-a word-b))
    (cond
      [(equal? i j) best]
      [else
       (if (> (string-length d)
              (string-length best))
           d
           best)])))
