(define lat? ; list of atoms
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? (quote ()))
(lat? (quote (bacon and eggs)))
(lat? (quote (bacon (and eggs))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) 
                (member? a (cdr lat)))))))

(member? (quote tea) (quote (coffee tea or milk)))
(member? (quote poached) (quote (fried eggs and scrambled eggs)))

(define rember ; remove member
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(rember (quote and) (quote (bacon lettuce and tomato)))
(rember (quote toast) (quote (bacon lettuce and tomato)))
(rember (quote sauce) (quote (soy sauce and tomato sauce)))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) 
                  (firsts (cdr l)))))))

(firsts (quote ((a b) (c d) (e f))))
(firsts (quote (((five plums) four) 
                (eleven green oranges) 
                ((no) more))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR (quote topping) (quote fudge) (quote (ice cream with fudge for dessert)))
(insertR (quote jalapeno) (quote and) (quote (tacos tamales and salsa)))
(insertR (quote e) (quote d) (quote (a b c d f g d h)))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst (quote topping) (quote fudge) (quote (ice cream with fudge for dessert)))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 (quote vanilla) (quote chocolate) (quote banana)
        (quote (banana ice cream with chocolate topping)))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(multirember (quote cup) (quote (coffee cup tea cup and hick cup)))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old 
                                 (cons new 
                                       (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new 
                                 (cons old 
                                       (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
