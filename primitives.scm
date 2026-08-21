; quote
(quote atom)
(quote 1942)
(quote (atom))
(quote (atom turkey or))
(quote ((atom turkey) or))
(quote ())
(quote (() () () ()))

; car
(car (quote (a b c)))
(car (quote ((a b c) x y z)))
(car (quote hotdog)) ; Error
(car (quote ())) ; Error
(car (quote (((hotdogs)) (and) (pickle) relish)))
(car (car (quote (((hotdogs)) (and)))))

; cdr
(cdr (quote (a b c)))
(cdr (quote ((a b c) x y z)))
(cdr (quote (hamburger)))
(cdr (quote ((x) t r)))
(cdr (quote hotdogs)) ; Error
(cdr (quote ())) ; Error

(car (cdr (quote ((b) (x y) ((c))))))
(cdr (cdr (quote ((b) (x y) ((c))))))
(cdr (car (quote (a (b (c)) d)))) ; Error

; cons
(cons (quote peanut) (quote (butter and jelly)))
(cons (quote (banana and)) (quote (peanut butter and jelly)))
(cons (quote ((help) this)) (quote (is very ((hard) to learn))))
(cons (quote (a b (c))) (quote ()))
(cons (quote ((a b c))) (quote b)) ; Error
(cons (quote a) (quote b)) ; Error

(cons (quote a) (car (quote ((b) c d))))
(cons (quote a) (cdr (quote ((b) c d))))

; null?
(null? (quote ()))
(null? (quote (a b c)))
(null? (quote spaghetti)) ; Error

; atom?
(atom? (quote Harry))
(atom? (quote (Harry had a heap of apples)))
(atom? (car (quote (Harry had a heap of apples))))
(atom? (cdr (quote (Harry had a heap of apples))))
(atom? (cdr (quote (Harry))))
(atom? (car (cdr (quote (swing low sweet cherry oat)))))
(atom? (car (cdr (quote (swing (low sweet) cherry oat)))))

; eq?
(eq? (quote Harry) (quote Harry))
(eq? (quote margarine) (quote butter))
(eq? (quote ()) (quote (strawberry))) ; Error
(eq? 6 7) ; Error

(eq? (car (quote (Mary had a little lamb chop))) (quote Mary))
(eq? (cdr (quote (soured milk))) (quote milk)) ; Error

(define l (quote (beans beans we need jelly beans)))
(eq? (car l) (car (cdr l)))
