; quote
(quote atom)
(quote 1942)
(quote (atom))
(quote (atom turkey or))
(quote ((atom turkey) or))
(quote ())
(quote (() () () ()))

; '
'atom
'1942
'(atom)
'(atom turkey or)
'((atom turkey) or)
'()
'(() () () ())

; car
(car '(a b c))
(car '((a b c) x y z))
(car 'hotdog) ; Error
(car '()) ; Error
(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdogs)) (and))))

; cdr
(cdr '(a b c))
(cdr '((a b c) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
(cdr 'hotdogs) ; Error
(cdr '()) ; Error

(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))
(cdr (car '(a (b (c)) d))) ; Error

; cons
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons '((a b c)) 'b) ; Error
(cons 'a 'b) ; Error

(cons 'a (car '((b) c d)))
(cons 'a (cdr '((b) c d)))

; null?
(null? '())
(null? '(a b c))
(null? 'spaghetti) ; Error

; atom?
(atom? 'Harry)
(atom? '(Harry had a heap of apples))
(atom? (car '(Harry had a heap of apples)))
(atom? (cdr '(Harry had a heap of apples)))
(atom? (cdr '(Harry)))
(atom? (car (cdr '(swing low sweet cherry oat))))
(atom? (car (cdr '(swing (low sweet) cherry oat))))

; eq?
(eq? 'Harry 'Harry)
(eq? 'margarine 'butter)
(eq? '() '(strawberry)) ; Error
(eq? 6 7) ; Error

(eq? (car '(Mary had a little lamb chop)) 'Mary)
(eq? (cdr '(soured milk)) 'milk) ; Error

(define l '(beans beans we need jelly beans))
(eq? (car l) (car (cdr l)))
