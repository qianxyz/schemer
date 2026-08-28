(define first (lambda (p) (car p)))
(define second (lambda (p) (car (cdr p))))
(define third (lambda (p) (car (cdr (cdr p)))))
(define build (lambda (s1 s2) (cons s1 (cons s2 '()))))

; an entry is a pair of lists of equal length
; and (first entry) is a set
'((appetizer entree beverage)
  (pate      boeuf  vin     ))
'((appetizer entree beverage)
  (beer      beer   beer     ))
'((beverage  dessert)
  ((food is) (number one with us)))

; build entry from a set of names and a list of values
(define new-entry build)

; entry-f takes one arg `name`
; called when `name` cannot be found in `entry`
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      entry-f)))
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help name
              (cdr names)
              (cdr values)
              entry-f)))))

(lookup-in-entry
  'entree
  '((appetizer entree beverage)
    (food tastes good))
  (lambda (name) (build 'not-found name)))
(lookup-in-entry
  'dessert
  '((appetizer entree beverage)
    (food tastes good))
  (lambda (name) (build 'not-found name)))

; a table/env is a list of entries
'(((appetizer entree beverage)
   (pate      boeuf  vin     ))
  ((beverage  dessert)
   ((food is) (number one with us))))

; add an entry to the front of the table
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
              (lambda (name)
                (lookup-in-table name (cdr table) table-f)))))))

(lookup-in-table 'entree
  '(((entree dessert)
     (spaghetti spumoni))
    ((appetizer entree beverage)
     (food tastes good)))
  (lambda (name) (build 'not-found name)))

; types of expressions in Scheme:
; *const:       numbers, #t, #f, primitive procedures
; *quote:       (quote x)
; *identifier:  symbols
; *lambda:      (lambda (x) ...)
; *cond:        (cond ((test?) x) (else y))
; *application: (func arg1 arg2)
; (no need for `define`)
; each corresponds to an action (function)
; that evaluates this specific type of expression

; functions that map types to actions
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

; eval
(define value (lambda (e) (meaning e '()))) ; '(): empty table
(define meaning (lambda (e table) ((expression-to-action e) e table)))

; action *const
; n -> n, #t -> #t, #f -> #f
; primitive procedures like car -> '(primitive car)
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(value '6)
(value '#t)
(value '#f)
(value 'car)

; action *quote
(define *quote
  (lambda (e table)
    (text-of e)))
(define text-of second) ; '(quote nothing) -> 'nothing

(value '(quote nothing))

; action *identifier
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))
(define initial-table
  (lambda (name) (car '()))) ; panic at lookup failure

(meaning 'x '(((x) (6))))
; (value 'x) ; error

; action *lambda: tagging 'non-primitive
; storing the table, formal args and body
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))
; for field retrieval
(define table-of first)
(define formals-of second)
(define body-of third)

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))
(table-of (second (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))))
(formals-of (second (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))))
(body-of (second (meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))))

; action *cond: (cond lines) where lines is a list of (question answer)
; no (null? lines) so no matching branch is runtime error
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
(define else? (lambda (x) (and (atom? x) (eq? x 'else))))
(define question-of first)
(define answer-of second)
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t)) ((klatsch party) (5 (6)))))
(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#f)) ((klatsch party) (5 (6)))))

; action *application
; eval the function, eval all its arguments
; then apply the arguments to the function
(define evlis ; eval on the list
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table)
                  (evlis (cdr args) table))))))
(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))
(define function-of car)
(define arguments-of cdr)

; to eval a function, there are two types:
; ('primitive primitive-name)
; ('non-primitive (table formals body)) ; closure record
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons) (cons (first vals) (second vals)))
      ((eq? name 'car) (car (first vals)))
      ((eq? name 'cdr) (cdr (first vals)))
      ((eq? name 'null?) (null? (first vals)))
      ((eq? name 'eq?) (eq? (first vals) (second vals)))
      ((eq? name 'atom?) (:atom? (first vals))) ; careful!
      ((eq? name 'zero?) (zero? (first vals)))
      ((eq? name 'add1) (add1 (first vals)))
      ((eq? name 'sub1) (sub1 (first vals)))
      ((eq? name 'number?) (number? (first vals))))))
; the book defines atom? as anything that's not a list
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry (formals-of closure) vals)
               (table-of closure)))))

(apply-closure '((((u v w)
                   (1 2 3))
                  ((x y z)
                   (4 5 6)))
                 (x y)
                 (cons z x))
               '((a b c) (d e f)))

(value '(car (quote (a b c))))
(value '(car '(a b c))) ; works too since ' -> quote happens in host language
(value '(add1 6))
(value '((lambda (nothing)
           (cons nothing (quote ())))
         (quote (from nothing comes something))))
(value '((lambda (nothing)
           (cond
             (nothing (quote something))
             (else (quote nothing))))
         #t))

(value '(((lambda (le)
            ((lambda (f) (f f))
             (lambda (f)
               (le (lambda (x) ((f f) x))))))
          (lambda (length)
            (lambda (l)
              (cond
                ((null? l) 0)
                (else (add1 (length (cdr l))))))))
         '(a b c d e)))
