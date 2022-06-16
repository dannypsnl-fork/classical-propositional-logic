#lang nanopass
(provide KF->clausal)
(require "first-order-logic.rkt")

(define-language KF0
  (extends KF)
  (Expr (e)
        (- (→ e0 e1))))
(define-pass remove-implication : KF (e) -> KF0 ()
  (T : Expr (e) -> Expr ()
     [(→ ,[e0] ,[e1])
      `(∨ (¬ ,e0) ,e1)]))

(define KF->clausal (compose remove-implication))

(module+ main
  (define all (compose KF->clausal
                       parse-KF))

  (all '(→ A (→ B C)))
  )