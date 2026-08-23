(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* (quote cup) 
         (quote ((coffee) cup ((tea) cup)
                 (and (hick)) cup)))
(rember* (quote sauce) 
         (quote (((tomato sauce))
                 ((bean) sauce)
                 (and ((flying)) sauce))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* (quote roast) (quote chuck)
          (quote ((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood)))

; copied over
(define +
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (+ n (sub1 m)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* (quote banana)
        (quote ((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* (quote orange) (quote banana)
        (quote ((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* (quote pecker) (quote chuck)
          (quote ((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood)))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* (quote chips) (quote ((potato) (chips ((with) fish) (chips)))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost (quote ((potato) (chips ((with) fish) (chips)))))
(leftmost (quote (((hot) (tuna (and))) cheese)))
; (leftmost (quote (((() four)) 17 (seventeen))))
; (leftmost (quote ()))

; copied over
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define eqan? ; equal atoms and numbers
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

; (define eqlist?
;   (lambda (l1 l2)
;     (cond
;       ((and (null? l1) (null? l2)) #t)
;       ((and (null? l1) (atom? (car l2))) #f)
;       ((null? l1) #f)
;       ((and (atom? (car l1)) (null? l2)) #f)
;       ((and (atom? (car l1)) (atom? (car l2)))
;        (and (eqan? (car l1) (car l2))
;             (eqlist? (cdr l1) (cdr l2))))
;       ((atom? (car l1)) #f)
;       ((null? l2) #f)
;       ((atom? (car l2)) #f)
;       (else (and (eqlist? (car l1) (car l2))
;                  (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? (quote (strawberry ice cream))
         (quote (strawberry ice cream)))
(eqlist? (quote (strawberry ice cream))
         (quote (strawberry cream ice)))
(eqlist? (quote (banana ((split))))
         (quote ((banana) (split))))
(eqlist? (quote (beef ((sausage)) (and (soda))))
         (quote (beef ((salami)) (and (soda)))))
(eqlist? (quote (beef ((sausage)) (and (soda))))
         (quote (beef ((sausage)) (and (soda)))))

(define equal? ; any S-expression
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; shadows the previous define
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2))))))) ; can also be equal?

(eqlist? (quote (strawberry ice cream))
         (quote (strawberry ice cream)))
(eqlist? (quote (strawberry ice cream))
         (quote (strawberry cream ice)))
(eqlist? (quote (banana ((split))))
         (quote ((banana) (split))))
(eqlist? (quote (beef ((sausage)) (and (soda))))
         (quote (beef ((salami)) (and (soda)))))
(eqlist? (quote (beef ((sausage)) (and (soda))))
         (quote (beef ((sausage)) (and (soda)))))

(define rember ; any S-expression
  (lambda (s l)
    (cond
      ((null? l) (quote ()))
      ((equal? s (car l)) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))
