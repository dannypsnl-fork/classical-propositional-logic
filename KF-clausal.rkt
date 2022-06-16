#lang nanopass
(provide KF->clausal
         KF3)
(require "first-order-logic.rkt"
         "stable.rkt"
         (only-in list-util zip))


(define-pass subst : KF (e subst-map) -> KF ()
  (Expr : Term (e) -> Term ()
        [,x (guard (assoc x subst-map))
            (cdr (assoc x subst-map))]))
(define-pass uniquify : KF (e) -> KF ()
  (definitions
    (define (gen-newvs vs)
      (map gensym vs)))
  (Expr : Expr (e) -> Expr ()
        [(∃ (,x ...) ,[e])
         (define new-vs (gen-newvs x))
         `(∃ (,new-vs ...) ,(subst e (zip x new-vs)))]
        [(∀ (,x ...) ,[e])
         (define new-vs (gen-newvs x))
         `(∀ (,new-vs ...) ,(subst e (zip x new-vs)))]))

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

(define-language KF2
  (extends KF1)
  (Prenex (prenex)
          (- (∃ (x* ...) prenex))))
(define-pass subst-skolem : KF2 (e subst-map) -> KF2 ()
  (T : Term (e) -> Term ()
     [,x (guard (assoc x subst-map))
         (cdr (assoc x subst-map))]))
(define-pass skolem : KF1 (e) -> KF2 ()
  (T : Prenex (e) -> Prenex ()
     [(∃ (,x* ...) ,[e])
      (subst-skolem e (zip x* (map (lambda (x) `(,(gensym 'Skolem) ,x)) x*)))]))

(define-language KF3
  (extends KF2)
  (Expr (e)
        (- prenex)
        (+ a)))
(define-pass remove-∀ : KF2 (e) -> KF3 ()
  (T : Expr (e) -> Expr ()
     [(∀ (,x* ...) ,[e]) e]))

(define-pass ~~>clausal : KF3 (e) -> KF3 ()
  (T : Expr (e) -> Expr ()
     [(∨ ,a2 (∧ ,a0 ,a1)) `(∧ (∨ ,a0 ,a2) (∨ ,a1 ,a2))]
     [(∨ (∧ ,a0 ,a1) ,a2) `(∧ (∨ ,a0 ,a2) (∨ ,a1 ,a2))]
     [(¬ (∧ ,a0 ,a1)) `(∨ (¬ ,a0) (¬ ,a1))]
     [(¬ (∨ ,a0 ,a1)) `(∧ (¬ ,a0) (¬ ,a1))]
     [(¬ ,⊥) '⊤]
     [(¬ ,⊤) '⊥]
     [(∨ ,⊤ ,a) ⊤]
     [(∨ ,a ,⊤) ⊤]
     [(¬ (¬ ,a)) a])
  (stable e T unparse-KF3))

(define KF->clausal (compose ~~>clausal
                             remove-∀
                             skolem
                             remove-implication
                             prenex-form
                             lift-quantifier
                             uniquify))

(define all (compose unparse-KF3
                     KF->clausal
                     parse-KF))

(module+ test
  (require rackunit)
  (check-equal? (all '(→ A (→ B C)))
                '(∨ (¬ A) (∨ (¬ B) C)))
  (check-equal? (all '(∨ A (∧ B C)))
                '(∧ (∨ B A) (∨ C A))))

(module+ main
  (all '(∃ (a) (∀ (a) (D a))))
  (all '(∃ (a) (∀ (b) (D a)))))
