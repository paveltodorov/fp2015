#lang racket
(define( series a b n)
 (define( series-help a b i)
 (cond
    [  (= n 1) a]
    [  (= n 2) b]
    [ (> i n)  b]
    [ ( series-help b (+ a b) (+ i 1))]))
(series-help a b 3))
(define(fibonacci n) (series 1 1 n))
(define(lucas n) (series 2 1 n))
(define (summed-member n)  (+ (lucas n) (fibonacci n)))
(define (nth-fibonacci-sum n)
 (define (nth-fibonacci-sum-help n res)
   (cond
     [ (= n 0) res]
     [ else (nth-fibonacci-sum-help (- n 1) (+ res (fibonacci n)))]))
  (nth-fibonacci-sum-help n 0))
(define (nth-lucas-sum n)
 (define (nth-lucas-sum-help n res)
   (cond
     [ (= n 0) res]
     [ else (nth-lucas-sum-help (- n 1) (+ res (lucas n)))]))
  (nth-lucas-sum-help n 0))
(define (lucas-fib-diff n)
  (-(lucas n) (fibonacci n)))
