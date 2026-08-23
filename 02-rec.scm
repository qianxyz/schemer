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
