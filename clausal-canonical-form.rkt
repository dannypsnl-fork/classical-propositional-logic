#lang nanopass
(require "classical-logic.rkt")

(define-pass K->clausal : K (e) -> K ()
  (T : Expr (e) -> Expr ()
     [(→ ,[e0] ,[e1]) `(∨ (¬ ,e0) ,e1)]
     [(∨ ,[e2] (∧ ,[e0] ,[e1])) `(∧ (∨ ,e0 ,e2) (∨ ,e1 ,e2))]
     [(∨ (∧ ,[e0] ,[e1]) ,[e2]) `(∧ (∨ ,e0 ,e2) (∨ ,e1 ,e2))]
     [(¬ (∧ ,[e0] ,[e1])) `(∨ (¬ ,e0) (¬ ,e1))]
     [(¬ (∨ ,[e0] ,[e1])) `(∧ (¬ ,e0) (¬ ,e1))]
     [(¬ ,⊥) '⊤]
     [(¬ ,⊤) '⊥]
     [(∨ ,⊤ ,e) ⊤]
     [(∨ ,e ,⊤) ⊤]
     [(¬ (¬ ,[e])) e])
  (let loop ([e-s e])
    (define e-t (T e-s))
    (if (equal? (unparse-K e-t) (unparse-K e-s))
        e-t
        (loop e-t))))

(define-language K-canonical
  (extends K)
  (Expr (e)
        (- (∨ e0 e1)
           (∧ e0 e1))
        (+ (∨ e* ...)
           (∧ e* ...))))
(define-pass clausal->canonical : K (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ (∨ ,[e0] ,[e1]) ,[e2]) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] (∨ ,[e1] ,[e2])) `(∨ ,e0 ,e1 ,e2)]
     [(∧ (∧ ,[e0] ,[e1]) ,[e2]) `(∨ ,e0 ,e1 ,e2)]
     [(∧ ,[e0] (∧ ,[e1] ,[e2])) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] ,[e1]) `(∨ ,e0 ,e1)]
     [(∧ ,[e0] ,[e1]) `(∧ ,e0 ,e1)]))
(define-pass canonical->cnf : K-canonical (e) -> * ()
  (Pos : Expr (e) -> * ()
       [,x (set (cons #t x))]
       [,⊤ (set)]
       [,⊥ (set (set))]
       [(→ ,e0 ,e1) (set-union (Neg e0) (Pos e1))]
       [(∧ ,e* ...) (apply set (map Pos e*))]
       [(∨ ,e* ...) (apply set-union (map Pos e*))]
       [(¬ ,e) (Neg e)])
  (Neg : Expr (e) -> * ()
       [,x (set (cons #f x))]
       [,⊤ (set (set))]
       [,⊥ (set)]
       [(→ ,e0 ,e1) (set (Pos e0) (Neg e1))]
       [(∧ ,e* ...) (apply set-union (map Neg e*))]
       [(∨ ,e* ...) (apply set (map Neg e*))]
       [(¬ ,e) (Pos e)])
  (Pos e))

(module+ main
  (define t (parse-K '(→ (∨ A B) (¬ (∧ (¬ A) (¬ B))))))
  (define t2 (parse-K '(∨ (¬ (∨ (¬ X) Y))
                          (∨ (¬ Y) Z))))

  (clausal->canonical (K->clausal t))
  ; FIXME: it haven't removed twice  ¬Y from the result, have to do more
  (clausal->canonical (K->clausal t2))

  (define K->cnf (compose canonical->cnf
                          clausal->canonical
                          K->clausal))

  (K->cnf t)
  (K->cnf t2))
