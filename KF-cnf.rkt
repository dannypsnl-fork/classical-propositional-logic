#lang nanopass
(provide KF->cnf)
(require "first-order-logic.rkt"
         "KF-canonical.rkt")

(define-language KF-check
  (extends KF-canonical)
  (Expr (e)
        (-  t
            ⊤
            ⊥
            (∧ e* ...)
            (∨ e* ...)
            (¬ e))
        (+ c))
  (Conjunction (c)
               (+ (∧ d* ...)
                  d))
  (Disjunction (d)
               (+ (∨ lit* ...)
                  lit))
  (Literal (lit)
           (+ t
              (¬ lit)
              ⊤
              ⊥)))
(define-pass form-canonical : KF-canonical (e) -> KF-check ()
  (T : Expr (e) -> Expr ()))

(define-pass canonical->cnf : KF-check (e) -> * ()
  (L : Literal (e) -> * ()
     [(¬ ,lit) (cons #f (unparse-KF-check lit))]
     [else (cons #t (unparse-KF-check e))])
  (D : Disjunction (e) -> * ()
     [(∨ ,lit* ...)
      (list->set (map L lit*))]
     [,⊤ #f]
     [,⊥ (set)]
     [,lit (set (L lit))])
  (C : Expr (e) -> * ()
     [(∧ ,d* ...)
      (map D d*)]
     [,d (list (D d))])
  (filter identity (C e)))

(define KF->cnf (compose canonical->cnf
                         form-canonical
                         KF->canonical
                         parse-KF))

(module+ main
  (KF->cnf '(∨ A ⊤))
  (KF->cnf '⊥)
  (KF->cnf 'A)
  (KF->cnf '(∨ A B))
  (KF->cnf '(∧ (∀ (a) (→ (Red a)
                         (Sweet a)))
               (Red Apple))))
