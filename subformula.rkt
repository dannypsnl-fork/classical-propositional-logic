#lang racket/base
(require "classical-logic.rkt"
         nanopass/base
         racket/set)

(define-pass subformula : K (e) -> * ()
  (S : Expr (e) -> * ()
     [,x (set x)]
     [,⊤ (set ⊤)]
     [,⊥ (set ⊥)]
     [(→ ,e0 ,e1)
      (set-union (set e) (S e0) (S e1))]
     [(∧ ,e0 ,e1)
      (set-union (set e) (S e0) (S e1))]
     [(∨ ,e0 ,e1)
      (set-union (set e) (S e0) (S e1))]
     [(¬ ,e) (set-union (set e) (S e))]))

(module+ main
  (subformula (parse-K '(→ (∨ A B) (¬ (∧ (¬ A) (¬ B)))))))
