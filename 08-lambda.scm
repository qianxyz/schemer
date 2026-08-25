(load "04-nums.scm")  ; for =, length, *, /
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

(load "06-reprs.scm") ; for operator, 1st-sub-exp, 2nd-sub-exp

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else ^))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value (1st-sub-exp nexp))
        (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(multiremberT (eq?-c 'tuna) '(shrimp salad tuna salad and tuna))

; (multirember&co a lat col) = (col L S)
; where L is lat with all a's removed
;       S is the list of all removed a's
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? a (car lat))
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

(multirember&co 'tuna '() list)
(multirember&co 'tuna '(tuna) list)
(multirember&co 'tuna '(and tuna) list)
(multirember&co 'tuna '(strawberries tuna and swordfish) list)
(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                (lambda (x y) (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL (car lat))
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat))
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))))))

; (multiinsertLR&co new oldL oldR lat col) = (col E L R)
; where E is the resulting list
;       L is # of left inserts
;       R is # of right inserts
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat L R) (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
         (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))

(multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list)

(define even? (lambda (n) (= n (* 2 (/ n 2)))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

; (evens-only*&co l col) = (col E P S)
; where E is l with evens only
;       P is the product of all evens
;       S is the sum of all odds
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
            (lambda (newl p s) (col (cons (car l) newl) (* p (car l)) s))))
         (else
          (evens-only*&co (cdr l)
            (lambda (newl p s) (col newl p (+ s (car l))))))))
      (else
       (evens-only*&co (car l)
         (lambda (al ap as)
           (evens-only*&co (cdr l)
             (lambda (dl dp ds)
               (col (cons al dl)
                    (* ap dp)
                    (+ as ds))))))))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
  (lambda (newl product sum)
    (cons sum (cons product newl))))
