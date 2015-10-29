#lang racket
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (simplify-frac pair)
  (define a(cons ( / (fst pair) (gcd (fst pair) (snd pair))) ( / (snd pair) (gcd (fst pair) (snd pair)))))
  a)

(define (add-frac frac1 frac2)
      (define a (cons ( + (* (fst frac1) (snd frac2) ) (* (fst frac2) (snd frac1) ))  (* (snd frac1) (snd frac2) )))
      (simplify-frac a))

(define (substract-frac frac1 frac2)
   (define a (cons ( - (* (fst frac1) (snd frac2) ) (* (fst frac2) (snd frac1) ))  (* (snd frac1) (snd frac2) )))
      (simplify-frac a))

(define (mult-frac frac1 frac2)
   (define a (cons (* (fst frac1) (fst frac2) ) (* (snd frac1) (snd frac2) )   ))
      (simplify-frac a))
  
