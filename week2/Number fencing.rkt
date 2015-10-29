#lang racket
(define (string-repeat str n)
(define (string-repeat-help str n res)
  (cond
  [ (= n 0) res ]
  [(string-repeat-help str (- n 1) (string-append res str ))]))
  (string-repeat-help str n ""))
(define (fence n)
(string-append "{" (string-repeat "-" (round( + 1 (log n)))) ">" (~a n) "<"(string-repeat "-" (round(+ 1 (log n)))) "}")  )
