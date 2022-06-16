#lang nanopass
(provide KF->canonical
         KF-canonical)
(require "KF-clausal.rkt")

(define (top? e) (eq? e '⊤))
(define (bot? e) (eq? e '⊥))
(define-language KF4
  (terminals
   (symbol (x))
   (top (⊤))
   (bot (⊥)))
  (Expr (e)
        t #| term |#
        ⊤ #| truth |#
        ⊥ #| falsity |#
        (∧ e0 e1) #| conjuction |#
        (∨ e0 e1) #| disjunction |#
        (¬ e) #| negation |#)
  (Term (t)
        x #| propositional-variable |#
        (x t* ...) #| predicate |#))

(define-pass convert : KF3 (e) -> KF4 ()
  (T : Expr (e) -> Expr ()))

(define-language KF-canonical
  (extends KF4)
  (Expr (e)
        (- (∨ e0 e1)
           (∧ e0 e1))
        (+ (∨ e* ...)
           (∧ e* ...))))
(define-pass clausal-fuse-clause : KF4 (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ (∨ ,[e0] ,[e1]) ,[e2]) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] (∨ ,[e1] ,[e2])) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] ,[e1]) `(∨ ,e0 ,e1)]
     [(∧ (∧ ,[e0] ,[e1]) ,[e2]) `(∧ ,e0 ,e1 ,e2)]
     [(∧ ,[e0] (∧ ,[e1] ,[e2])) `(∧ ,e0 ,e1 ,e2)]
     [(∧ ,[e0] ,[e1]) `(∧ ,e0 ,e1)]))

(define-pass remove-same-twice-from-clause : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[e*] ...) `(∨ ,(remove-duplicates e*) ...)]))

(define-pass eliminate-bottom : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[e*] ...)
      (define new-e* (remove* '(⊥) e*))
      (cond
        [(empty? new-e*) '⊥]
        [(= (length new-e*) 1) (first new-e*)]
        [else `(∨ ,new-e* ...)])]))

(define-pass neg : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(¬ ,e) e]
     [else `(¬ ,e)]))
(define-pass offset-literal-and-its-negation : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[e*] ...)
      (define new-e* (remove* (map neg e*) e*))
      (cond
        [(empty? new-e*) '⊤]
        [(= (length new-e*) 1) (first new-e*)]
        [else `(∨ ,new-e* ...)])]))

(define KF->canonical (compose offset-literal-and-its-negation
                               eliminate-bottom
                               remove-same-twice-from-clause
                               clausal-fuse-clause
                               convert
                               KF->clausal))

(module+ test
  (require rackunit)
  (require "first-order-logic.rkt")

  (define all (compose unparse-KF-canonical
                       KF->canonical
                       parse-KF))

  (check-equal? (all '(∨ A (∨ B C)))
                '(∨ A B C))
  (check-equal? (all '(∨ A (¬ A)))
                '⊤)
  (check-equal? (all '(∨ A (∨ (¬ A) B)))
                'B))
