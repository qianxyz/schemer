(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(load "04-nums.scm") ; for pick

(define keep-looking
  (lambda (a sorn lat) ; symbol or number
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))
; (looking 'caviar '(7 2 4 7 5 6 3)) ; does not terminate
; (looking 'caviar '(7 1 2 caviar 5 6 3)) ; does not terminate
; looking is a partial function

(define eternity ; not terminating for any input
  (lambda (x) (eternity x)))

(load "07-rels.scm") ; for pair utils

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))
(shift '((a b) (c d)))

(define align
  (lambda (pora) ; pair or atom
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

; assuming input is a binary tree
; align to right leaning (a (b (c (d (...)))))
(align '((a b) c))
(align '((a b) (c d)))
(align '(((a b) (c d)) (e f)))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* 2 (weight* (first pora)))
               (weight* (second pora)))))))

(weight* '((a b) c))
(weight* '(a (b c)))
; (weight* pora) > (weight* (shift pora))
; so the argument to align becomes smaller
; hence align will always terminate i.e. total

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
; (shuffle '((a b) (c d))) ; does not terminate
; shuffle is not total

; copied over
(define even? (lambda (n) (= n (* 2 (/ n 2)))))

(define C ; Collatz conjecture
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (add1 (* 3 n)))))))

(define A ; Ackermann function (total)
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

; halting problem
; (define will-stop? (lambda (f) ...))
; (will-stop? f) = <if (f '()) will terminate>
; e.g. (will-stop? length) = #t
;      (will-stop? eternity) = #f
;
; (define last-try
;   (lambda (x)
;     (and (will-stop? last-try)
;          (eternity x))))
; if (will-stop? last-try) = #f:
;   then (last-try '()) evaluates to #f (and stops), contradiction
; if (will-stop? last-try) = #t:
;   then (last-try '()) evaluates to (eternity '()), contradiction

; (define length
;   (lambda (lat)
;     (cond
;       ((null? lat) 0)
;       (else (add1 (length (cdr lat)))))))
; note that the recursive function must be given a name,
; but in lambda calculus all functions are anonymous.
; how to achieve recursion without `define`?

; this only works for empty list
; (define length0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))
; )

; s/eternity/length0 to make length<=1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
      ( ; length0
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (eternity (cdr l))))))
       (cdr l))))))

; s/eternity/length0 again to make length<=2
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1
      ((lambda (l)
         (cond
           ((null? l) 0)
           (else (add1
             ((lambda (l)
                (cond
                  ((null? l) 0)
                  (else (add1 (eternity (cdr l))))))
              (cdr l))))))
       (cdr l))))))

; rewrite length0 by extracting eternity
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

; substitute eternity with the new length0 to get length<=1
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  eternity))

; substitute eternity with the new length0 to get length<=2
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)))

; this is `eternity` piped into some function repeatedly
; calling it `mk-length`, rewrite length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; length<=1 we apply mk-length again
((lambda (mk-length)
   (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; length<=2 we apply mk-length again
((lambda (mk-length)
   (mk-length
     (mk-length
       (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; back to length0
; since we don't care about the else branch
; we can swap eternity out by mk-length itself
; and for the inner lambda we rename length -> mk-length formally
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

; length<=1 is (mk-length (mk-length eternity))
; putting the argument to the inner lambda body
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l))))))))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l))))))))
 '(apple))

; now instead of eternity just keep passing mk-length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l))))))))

; extract (mk-length mk-length) to a lambda
; also since (mk-length mk-length) is a function
; we write it as (lambda (x) ((mk-length mk-length) x))
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (
; note how this part looks like length
    (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
;
    (lambda (x) ((mk-length mk-length) x)))))

; extract the middle part into a lambda
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x) ((mk-length mk-length) x))))))
;
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

; so the first lambda function
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x) ((mk-length mk-length) x))))))
; when we supply the ordinary function
; (lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l)))))))
; turns it into a recursive function

; this is defined as the Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
