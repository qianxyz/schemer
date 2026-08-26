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
