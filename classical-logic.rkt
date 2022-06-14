#lang racket/base
(provide K
         parse-K
         unparse-K)

(require nanopass/base)

(define (top? e) (eq? e '⊤))
(define (bot? e) (eq? e '⊥))
;;; Natural deduction of Classical logic
(define-language K
  (terminals
   (symbol (x))
   (top (⊤))
   (bot (⊥)))
  (Expr (e)
        x #| propositional-variable |#
        ⊤ #| truth |#
        ⊥ #| falsity |#
        (→ e0 e1) #| implication |#
        (∧ e0 e1) #| conjuction |#
        (∨ e0 e1) #| disjunction |#
        (¬ e) #| negation |#))

(define-parser parse-K K)
