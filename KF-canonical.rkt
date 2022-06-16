#lang nanopass
(provide KF->canonical)
(require "KF-clausal.rkt")

(define-language KF-canonical
  (extends KF3)
  (A (a)
     (- (∨ a0 a1)
        (∧ a0 a1))
     (+ (∨ a* ...)
        (∧ a* ...))))
(define-pass clausal-fuse-clause : KF3 (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ (∨ ,[a0] ,[a1]) ,[a2]) `(∨ ,a0 ,a1 ,a2)]
     [(∨ ,[a0] (∨ ,[a1] ,[a2])) `(∨ ,a0 ,a1 ,a2)]
     [(∧ (∧ ,[a0] ,[a1]) ,[a2]) `(∨ ,a0 ,a1 ,a2)]
     [(∧ ,[a0] (∧ ,[a1] ,[a2])) `(∨ ,a0 ,a1 ,a2)]
     [(∨ ,[a0] ,[a1]) `(∨ ,a0 ,a1)]
     [(∧ ,[a0] ,[a1]) `(∧ ,a0 ,a1)]))

(define-pass remove-same-twice-from-clause : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[a*] ...) `(∨ ,(remove-duplicates a*) ...)]))

(define-pass eliminate-bottom : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[a*] ...)
      (define new-e* (remove* '(⊥) a*))
      (cond
        [(empty? new-e*) '⊥]
        [(= (length new-e*) 1) (first new-e*)]
        [else `(∨ ,new-e* ...)])]))

(define-pass neg : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(¬ ,a) a]
     [else `(¬ ,e)]))
(define-pass offset-literal-and-its-negation : KF-canonical (e) -> KF-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,[a*] ...)
      (define new-e* (remove* (map neg a*) a*))
      (if (empty? new-e*)
          '⊤
          `(∨ ,new-e* ...))]))

(define KF->canonical (compose offset-literal-and-its-negation
                               eliminate-bottom
                               remove-same-twice-from-clause
                               clausal-fuse-clause
                               KF->clausal))

(module+ main
  (require "first-order-logic.rkt")

  (define all (compose unparse-KF-canonical
                       KF->canonical
                       parse-KF))

  (all '(∨ A (∨ B C)))
  )
