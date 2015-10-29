#lang racket
(define (cube-sums? n)
  (define (help1 a b)
       (cond
[(= a n) #f]
[  (= (+ (* a a a) (* b b b)) n)  #t]
[else(help1 (+ 1 a) b)]))
 (define(sum-of-cubes-help a b)
    (cond
      [ (> b n) #f]
      [(help1 a b) #t]
      [else (sum-of-cubes-help a (+ 1 b))]))
  (sum-of-cubes-help 1 1))
(define (count-cube-sums from to)
    (define  (count-cube-sums-help from res)
      ( cond
         [ (> from to)   res]
         [(cube-sums? from) (count-cube-sums-help (+ 1 from) (+ 1 res))]
         [ else (count-cube-sums-help (+ 1 from) res)]))
(count-cube-sums-help from 0))
