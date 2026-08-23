(atom? 14)

(add1 67)
(sub1 5)
(zero? 0)
(zero? 1492)

(define +
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (+ n (sub1 m)))))))

(+ 46 12)

(define -
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

(- 14 3)
(- 17 9)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup (quote (3 5 2 8)))
(addtup (quote (15 6 7 12 3)))

(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (* n (sub1 m)))))))

(* 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ (quote (3 7)) (quote (4 6)))
(tup+ (quote (3 7)) (quote (4 6 8 1)))
(tup+ (quote (3 7 8 1)) (quote (4 6)))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(> 12 133)
(> 120 11)
(> 3 3)

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))
; (define < (lambda (n m) (> m n)))

(< 4 6)
(< 8 3)
(< 6 6)

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (^ n (sub1 m)))))))

(^ 1 1)
(^ 2 3)
(^ 5 3)

(define /
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (/ (- n m) m))))))

(/ 15 4)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(length (quote (hotdogs with mustard sauerkraut and pickles)))
(length (quote (ham and cheese on rye)))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(pick 4 (quote (lasagna spaghetti ravioli macaroni meatball)))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 (quote (hotdogs with hot mustard)))

(number? (quote tomato))
(number? 76)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums (quote (5 pears 6 prunes 9 dates)))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums (quote (5 pears 6 prunes 9 dates)))

(define eqan? ; equal atoms and numbers
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one? (lambda (n) (= n 1)))
