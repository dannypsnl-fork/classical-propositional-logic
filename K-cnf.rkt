#lang nanopass
(provide K->cnf)
(require "K-canonical.rkt")

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

(define K->cnf (compose canonical->cnf
                        K->canonical))
