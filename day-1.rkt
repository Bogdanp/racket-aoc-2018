#lang racket

(call-with-input-file "day-1.txt"
  (lambda (in)
    (let loop ([n (read in)]
               [freq 0])
      (cond
        [(eof-object? n) freq]
        [else (loop (read in) (+ freq n))]))))
