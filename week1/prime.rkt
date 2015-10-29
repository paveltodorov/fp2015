#lang racket
(provide prime?)
(define (prime? n)
(define (prime-help n i )
( cond
  [ (> i (/ n 2)  ) #t]
  [ (zero? (remainder n i )) #f]
  [ else (prime-help n (+ i 1))]))
  (prime-help n 2 ))
