(load "05-stars.scm") ; for equal?

; rewritten with equal? instead of eq?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? a (car lat)) 
                (member? a (cdr lat)))))))
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apple peaches pears plum))
(set? '(apple 3 pear 4 9 apple 3 4))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(apple peach pear peach plum apple lemon peach))

; alternative definition
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset (multirember (car lat) (cdr lat))))))))

(makeset '(apple peach pear peach plum apple lemon peach))
(makeset '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings)
         '(5 hamburgers
           2 pieces of fried chicken and
           light duckling wings))
(subset? '(4 pounds of horseradish)
         '(four pounds of chicken and
           5 ounces of horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))

(difference '(stewed tomatoes and macaroni casserole)
            '(macaroni and cheese))

(define intersectall
  (lambda (l-set) ; assuming non-empty
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) 
                       (intersectall (cdr l-set)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))

; for pairs
(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define build (lambda (s1 s2) (cons s1 (cons s2 '()))))

; a rel is a set of pairs
; a function is a rel whose firsts is a set
(define firsts
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (first (car rel)) 
                  (firsts (cdr rel)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel)) 
                  (seconds (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one? ; same as fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))
(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))
(one-to-one? '((8 3) (4 2) (7 6) (6 2) (3 4)))
(one-to-one? '((8 3) (4 8) (7 6) (6 2) (3 4)))
(one-to-one? '((grape raisin)
               (plum prune)
               (stewed prune)))
(one-to-one? '((grape raisin)
               (plum prune)
               (stewed grape)))
