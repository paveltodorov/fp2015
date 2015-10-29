#lang racket
(provide string-reverse)
(define(string-reverse str)
   (define(string-reverse-help str n res)
     (cond
       [(= n 0) res]
       [(string-reverse-help str (- n 1) (string-append res (~a (string-ref str (- n 1)))))]))
    (string-reverse-help str  (string-length str) "" ))
(provide to-binary-string)
(define (to-binary-string n)
 (define (help1 n r )
 (cond
  [(= n 0) r ]
  [else  (help1 (quotient n 2)   (string-append r (~a (remainder n 2))))]))
  (if (= n 0) "0" ( string-reverse (help1 n ""))))
(define (from-binary-string str)
   (define (help2 str pow len res )
     (cond
       [ (= len 0) res  ]
       [ (help2 str (* pow 2) (- len 1)  ( + res (* pow (string->number (~a(string-ref str (- len 1)))))))]))
  (help2 str 1 (string-length str) 0 ))
