#lang racket
(define (prime? n)
(define (prime-help n i )
( cond
  [ (> i (/ n 2)  ) #t]
  [ (zero? (remainder n i )) #f]
  [ else (prime-help n (+ i 1))]))
  (prime-help n 2 ))
(define (add1 x) (+ 1 x))
(define (even? x) ( = (remainder x 2) 0))
(define (square x) (* x x))

(define (f p g h) (lambda (x)  (and (p (g x)) (p (h x)))))
