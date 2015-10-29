#lang racket
(define (nth-beast-number n)
(define(nth-beast-number-help i res)
  (cond
    [(= n i) res]
    [else (nth-beast-number-help (+ i 1) ( + (* 1000 res) 666))]))
(nth-beast-number-help 0 0))
