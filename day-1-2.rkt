#lang racket

(let outer ([freq 0]
            [seen (set)])
  (call-with-input-file "day-1-2.txt"
    (lambda (in)
      (let loop ([n (read in)]
                 [freq freq]
                 [seen seen])
        (cond
          [(eof-object? n)
           (outer freq seen)]

          [(set-member? seen freq) freq]

          [else (loop (read in) (+ freq n) (set-add seen freq))])))))
