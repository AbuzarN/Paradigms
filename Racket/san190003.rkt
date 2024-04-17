#lang racket
(define (divisible-by-x? x)
  (lambda (y) (if (= (modulo y x) 0)
                  #t
                  #f)))
(define (function-4 x)
  (x 4))

(define( my-map funct myList)
  (if (empty? myList)
      empty
      (cons (funct (first myList)) (my-map funct (rest myList)))))

(define (pair-up lst1 lst2)
  (if ( or (empty? lst1) (empty? lst2))
             '()
             (cons (list (car lst1) (car lst2)) (pair-up (cdr lst1) (cdr lst2))  )))

(define (classify funct lst)
  (define (classifyh lst lst1 lst2)
    (cond
      [(empty? lst) (list lst1 lst2) ]
      [(funct (first lst)) (classifyh (rest lst) (append lst1 (list (first lst))) lst2)]
      [else (classifyh (rest lst) lst1 (append lst2 (list (first lst))))]))
  (classifyh lst '() '()))


(define (luhn number)
  (define (recur lst itter)
    (cond
    [(null? lst) 0]
    [(odd? itter) (+ (summ(* 2 (car lst))) (recur (cdr lst) (+ itter 1) ) )  ]
    [else (+ (summ(* 1 (car lst))) (recur (cdr lst) (+ itter 1) ) )  ])
    )
  
  (define (summ in)
    (if (> 10 in)
        (+ in 0)
        (+ (quotient in 10) (remainder in 10) )))
  
  (define (luhnlst n)
  (cond
    [(= n 0) empty]
    [else (cons (remainder n 10) (luhnlst (quotient n 10)))]))

  (= (first (luhnlst number)) (- 10(modulo (recur (rest (luhnlst number)) 1) 10))))


(define (test n)
  (+ 2 (* 2 (first n)))
  )


(define (my-sorted? expr lst)
  (if ( or (null? list) (null? (cdr lst)))
      #t
      (if ( expr (first lst)  (second lst))
              (my-sorted? expr (cdr lst))
              #f)))

(define (my-flatten lst)
  (cond [(null? lst) empty] 
        [(list? (first lst)) 
        (append (my-flatten (first lst)) (my-flatten (rest lst)))]
        (else (cons (first lst) (my-flatten (rest lst))))))

(define (upper-threshold lst up)
	(cond ((null? lst) '())
              ((< (first lst) up) (cons (first lst) (upper-threshold (rest lst) up))) 
              (else (upper-threshold (rest lst) up))))

(define (my-list-ref lst index)
  (cond [(null? lst) (error "ERROR: Index out of bounds")]
        [(= 0 index) (first lst)]
        (else (my-list-ref (rest lst) (- index 1)))))

(define (deep-map funct lst)
  (cond [(null? lst) empty]
        [(list? (first lst)) (cons(deep-map funct (first lst)) (deep-map funct (rest lst) ))]
        (else (cons (funct (first lst)) (deep-map funct (rest lst)) ))))