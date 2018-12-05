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

(define polymer (string-trim (file->string "day-05-1.txt") "\n"))

(for/fold ([minimum +inf.0]
           #:result (inexact->exact minimum))
          ([i (in-range (char->integer #\a) (add1 (char->integer #\z)))])
  (define c (integer->char i))
  (min minimum
       (string-length
        (reduce-polymer
         (string-replace polymer (regexp (format "[~a~a]+" c (char-upcase c))) "")))))
