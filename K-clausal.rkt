#lang nanopass
(provide K->clausal)
(require "propositional-logic.rkt"
         "stable.rkt")

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
  (stable e T unparse-K))
