#lang racket
(require "binary.rkt")
(define (ispalindtome? str)   (equal? str (string-reverse str)))
(define (odd-1? str)
(define (odd-1-help?  i res )
  (cond
    [(< i 0) ( = (remainder res 2) 1)]
    [(equal? (~a (string-ref str i)) "1") (odd-1-help?  (- i 1) (+ 1 res))]
    [else(odd-1-help?  (- i 1) res)]))
  (odd-1-help? (- (string-length str) 1) 0 ))
(define(is-hack-number? n)
  (and (ispalindtome? (to-binary-string n)) (odd-1? (to-binary-string n))))
(define (next-hack n)
   (cond
     [(is-hack-number? (+ 1 n)) (+ 1 n)]
     [else(next-hack (+ 1 n))]))
