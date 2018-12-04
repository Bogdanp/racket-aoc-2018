#lang racket

(require gregor
         gregor/period
         gregor/time)

(struct log (timestamp message)
  #:transparent)

(define LOG-REGEXP #px"\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] (.+)")
(define (string->log s)
  (match-define (list _ year month day hour minute message) (regexp-match LOG-REGEXP s))
  (log (datetime (string->number year)
                 (string->number month)
                 (string->number day)
                 (string->number hour)
                 (string->number minute))
       message))

(define GUARD-ID-REGEXP #px"Guard #(\\d+) begins shift")
(define (string->guard-id s)
  (match-define (list _ id) (regexp-match GUARD-ID-REGEXP s))
  id)

(define logs (map string->log (sort (file->lines "day-04-2.txt") string<=?)))
(define frequencies
  (for/fold ([frequencies (hash)]
             [guard-id #f]
             [sleep-timestamp #f]
             #:result frequencies)
            ([l logs])
    (define message (log-message l))
    (cond
      [(string-prefix? message "Guard #")
       (values frequencies (string->guard-id message) #f)]

      [(string=? message "falls asleep")
       (values frequencies guard-id (log-timestamp l))]

      [(string=? message "wakes up")
       (match-define (period [minutes mdelta]) (period-between sleep-timestamp (log-timestamp l)))
       (define frequencies*
         (for/fold ([frequencies frequencies])
                   ([i (in-range mdelta)])
           (define k (->time (+period sleep-timestamp (period [minutes i]))))
           (hash-update frequencies k (curry cons guard-id) (list))))
       (values frequencies* guard-id #f)])))

(define (most-common xs)
  (define counts
    (for/fold ([counts (hash)])
              ([x xs])
      (hash-update counts x add1 0)))

  (first (sort (hash->list counts) (lambda (a b)
                                     (> (cdr a) (cdr b))))))

(define most-frequent-by-minute
  (for/fold ([appearances (hash)])
            ([(minute ids) (in-hash frequencies)])
    (hash-set appearances minute (most-common ids))))

(define most-frequently-sleepy-at-minute
  (first (sort (hash->list most-frequent-by-minute) (lambda (a b)
                                                      (> (cddr a)
                                                         (cddr b))))))

(* (->minutes (car most-frequently-sleepy-at-minute))
   (string->number (cadr most-frequently-sleepy-at-minute)))
