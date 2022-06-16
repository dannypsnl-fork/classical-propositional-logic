#lang nanopass
(provide KF->clausal)
(require "first-order-logic.rkt")

(define (stable e T unparse)
  (let loop ([e-s e])
    (define e-t (T e-s))
    (if (equal? (unparse e-t) (unparse e-s))
        e-t
        (loop e-t))))

(define-pass lift-quantifier : KF (e) -> KF ()
  (T : Expr (e) -> Expr ()
     [(∧ (∀ (,x* ...) ,e0) ,e1)
      `(∀ (,x* ...) (∧ ,e0 ,e1))]
     [(∧ ,e1 (∀ (,x* ...) ,e0))
      `(∀ (,x* ...) (∧ ,e0 ,e1))]
     [(∨ (∀ (,x* ...) ,e0) ,e1)
      `(∀ (,x* ...) (∨ ,e0 ,e1))]
     [(∨ ,e1 (∀ (,x* ...) ,e0))
      `(∀ (,x* ...) (∨ ,e0 ,e1))]
     [(∧ (∃ (,x* ...) ,e0) ,e1)
      `(∃ (,x* ...) (∧ ,e0 ,e1))]
     [(∧ ,e1 (∃ (,x* ...) ,e0))
      `(∃ (,x* ...) (∧ ,e0 ,e1))]
     [(∨ (∃ (,x* ...) ,e0) ,e1)
      `(∃ (,x* ...) (∨ ,e0 ,e1))]
     [(∨ ,e1 (∃ (,x* ...) ,e0))
      `(∃ (,x* ...) (∨ ,e0 ,e1))]
     [(→ (∃ (,x* ...) ,e0) ,e1)
      `(∀ (,x* ...) (→ ,e0 ,e1))]
     [(→ ,e0 (∃ (,x* ...) ,e1))
      `(∃ (,x* ...) (→ ,e0 ,e1))]
     [(→ (∀ (,x* ...) ,e0) ,e1)
      `(∃ (,x* ...) (→ ,e0 ,e1))]
     [(→ ,e0 (∀ (,x* ...) ,e1))
      `(∀ (,x* ...) (→ ,e0 ,e1))]
     [(¬ (∀ (,x* ...) ,e))
      `(∃ (,x* ...) (¬ ,e))]
     [(¬ (∃ (,x* ...) ,e))
      `(∀ (,x* ...) (¬ ,e))]
     [(∀ (,x* ...) ,⊤) '⊤]
     [(∃ (,x* ...) ,⊥)'⊥])
  (stable e T unparse-KF))

(define-language KF0
  (extends KF)
  (Expr (e)
        (- (∀ (x* ...) e)
           (∃ (x* ...) e)
           t
           ⊤
           ⊥
           (→ e0 e1)
           (∧ e0 e1)
           (∨ e0 e1)
           (¬ e))
        (+ prenex))
  (Prenex (prenex)
          (+ (∀ (x* ...) prenex)
             (∃ (x* ...) prenex)
             a))
  (A (a)
     (+ t
        ⊤
        ⊥
        (→ a0 a1)
        (∧ a0 a1)
        (∨ a0 a1)
        (¬ a))))
(define-pass prenex-form : KF (e) -> KF0 ()
  (T : Expr (e) -> Expr ()))

(define-language KF1
  (extends KF0)
  (A (a)
     (- (→ a0 a1))))
(define-pass remove-implication : KF0 (e) -> KF1 ()
  (T : A (e) -> A ()
     [(→ ,[a0] ,[a1])
      `(∨ (¬ ,a0) ,a1)]))

(define KF->clausal (compose remove-implication
                             prenex-form
                             lift-quantifier))

(module+ main
  (define all (compose KF->clausal
                       parse-KF))

  (all '(→ A (→ B C)))
  )