#lang racket

(define (reduce-polymer p)
  (let loop ([i 0]
             [s p])
    (cond
      [(>= i (sub1 (string-length s))) s]

      [(and (or (char=? (string-ref s i) (char-downcase (string-ref s (add1 i))))
                (char=? (char-downcase (string-ref s i)) (string-ref s (add1 i))))
            (not (char=? (string-ref s i) (string-ref s (add1 i)))))

       (loop (max 0 (sub1 i))
             (string-append (substring s 0 i)
                            (substring s (+ 2 i))))]

      [else
       (loop (add1 i) s)])))

(reduce-polymer (string-trim (file->string "day-05-2.txt") "\n"))
