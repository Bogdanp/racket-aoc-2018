#lang racket

(define (count-letters word)
  (for/fold ([counts (hash)])
            ([l (in-string word)])
    (hash-set counts l (add1 (hash-ref counts l 0)))))


(define (find-counts counts)
  (for/fold ([res (hash)])
            ([(_ c) (in-hash counts)])
    (hash-set res c (add1 (hash-ref res c 0)))))


(call-with-input-file "day-02-1.txt"
  (lambda (in)
    (let loop ([word (read in)]
               [twos 0]
               [threes 0])
      (cond
        [(eof-object? word) (* threes twos)]
        [else
         (define counts (find-counts (count-letters (symbol->string word))))

         (loop (read in)
               (if (hash-has-key? counts 2) (add1 twos) twos)
               (if (hash-has-key? counts 3) (add1 threes) threes))]))))
