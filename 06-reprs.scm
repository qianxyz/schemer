(load "04-nums.scm") ; for +, *, ^

'(n + 3)

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '1)
(numbered? '(3 + (4 * 5)))
(numbered? '(2 * sausage))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) '+)
       (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) '*)
       (* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
      (else
       (^ (value (car nexp))
          (value (car (cdr (cdr nexp)))))))))

(value '13)
(value '(1 + 3))
(value '(1 + (3 * 4)))
(value '(1 + (3 ^ 4)))

'(+ (* 3 6) (^ 8 2))

; helpers for prefix representations
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
(define operator
  (lambda (aexp)
    (car aexp)))

; shadows the previous define
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) '*)
       (* (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else
       (^ (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))

(value '1)
(value '(+ 3 4))
(value '(+ (* 3 6) (^ 8 2)))

; helpers for infix; shadows the previous define
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(value '13)
(value '(1 + 3))
(value '(1 + (3 * 4)))
(value '(1 + (3 ^ 4)))

'()         ; zero
'(())       ; one
'(() ())    ; two
'(() () ()) ; three

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons '() n)))
(define zub1
  (lambda (n)
    (cdr n)))

(define o+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (o+ n (zub1 m)))))))
