(define (sum numbers)
  (define (sum-iter numbers res)
    (cond
        [(empty? numbers) res]
         [else (sum-iter (cdr numbers) (+ res (car numbers)))]))
   (sum-iter numbers 0))

(define (member? x items)
  (cond
    [(empty? items) #f]
    [(equal? x (car items)) #t]
    [else(member? x(cdr items))]))

(define (lenght2 numbers)
   (define (lenght2-iter numbers res)
     (cond
       [(empty? numbers) res]
        [else (lenght2-iter (cdr numbers) (+ res 1))]))
  (lenght2-iter numbers 0))

(define (list-ref2 items n)
         (cond
           [(= n 0) (car items)]
           [(list-ref2 (cdr items) (- n 1))]))

(define (range2 a b)
  (define (range2-iter a b l)
    (cond
      [(< b a) l]
      [else (range2-iter  a (- b 1) (cons b l))]))
 (range2-iter a b '()))

(define (build-list2 n f)
(map f (range2 0 n)))

(define (append2-help l1 l2 )
  (cond
    [(empty?(cdr l1)) (cons (car l1) l2)]
    [else (cons (car l1) (append2-help (cdr l1) l2 ))]))

(define (reverse2 items)
  (define (reverse2-iter items res)
(cond
  [(empty? items) res]
  [else (reverse2-iter (cdr items) (cons (car items) res))]))
  (reverse2-iter items '()))

(define (take2 n items )
   (cond
     [(>= n (lenght2 items)) items]
     [(= n 0) '()]
     [else (cons (car items)  (take2 (- n 1) (cdr items)))]))

(define (drop2 n items)
  (cond
    [(> n (lenght2 items)) '()]
    [(= n 0) items]
    [else (drop2 (- n 1) (cdr items))]))

(define (take-while f items )
   (cond
     [(empty? items) '() ]
     [( not(f (car items))) '() ]
     [else (cons (car items)  (take-while f (cdr items) ))]))
   
(define (drop-while p items)
   (cond
     [(empty? items) items]
     [(not(p (car items))) items]
     [(p (car items))  (drop-while p (cdr items)) ]))
     
(define (number->list n)
  (define (number->list-iter n res)
  (cond
    [(= n 0) res ]
    [else(number->list-iter (quotient n 10)(cons (remainder n 10) res ))]))
  (number->list-iter n '()))

(define (list->number ns)
  (define (list->number-iter ns res)
    (cond
      [(empty? ns) res]
      [else(list->number-iter (cdr ns) (+ (* 10 res) (car ns)))]))
  (list->number-iter ns 0))
