(define lat? ; list of atoms
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? '())
(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) 
                (member? a (cdr lat)))))))

(member? 'tea '(coffee tea or milk))
(member? 'poached '(fried eggs and scrambled eggs))
