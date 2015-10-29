(define (reverse-int n)
  (define (reverse-help n res)
    (cond [(= n 0) res]
          [else (reverse-help (quotient n 10) (+ (* res 10) (remainder n 10 )))]))
  (reverse-help n 0))
(provide palindrome?)

(define (palindrome? n)
  (= n (reverse-int n)))

(define (p_score n)
  (cond
    [(palindrome? n ) 1]
    [(+ 1  (p_score (+ n (reverse-int n))))]))
