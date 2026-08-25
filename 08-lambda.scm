(load "04-nums.scm")  ; for =
(load "05-stars.scm") ; for equal?

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))

(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)
(eq?-salad 'tuna)
((eq?-c 'salad) 'salad)
((eq?-c 'salad) 'tuna)

; higher order
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

((rember-f eq?) 'tuna '(tuna salad is good))
((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL (lambda (new old l) (cons new (cons old l))))
(define seqR (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
(define insertL (insert-g (lambda (new old l) (cons new (cons old l)))))
(define insertR (insert-g (lambda (new old l) (cons old (cons new l)))))

(define seqS (lambda (new old l) (cons new l)))
(define subst (insert-g seqS))

(define seqrem (lambda (new old l) l))
(define rember (lambda (a l) ((insert-g seqrem) #f a l))) ; placeholder #f
