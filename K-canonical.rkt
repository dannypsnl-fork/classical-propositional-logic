#lang nanopass
(provide K->canonical
         K-canonical)
(require "propositional-logic.rkt"
         "K-clausal.rkt"
         "stable.rkt")

(define-language K-canonical
  (extends K)
  (Expr (e)
        (- (∨ e0 e1)
           (∧ e0 e1))
        (+ (∨ e* ...)
           (∧ e* ...))))
(define-pass clausal-fuse-clause : K (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ (∨ ,[e0] ,[e1]) ,[e2]) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] (∨ ,[e1] ,[e2])) `(∨ ,e0 ,e1 ,e2)]
     [(∧ (∧ ,[e0] ,[e1]) ,[e2]) `(∨ ,e0 ,e1 ,e2)]
     [(∧ ,[e0] (∧ ,[e1] ,[e2])) `(∨ ,e0 ,e1 ,e2)]
     [(∨ ,[e0] ,[e1]) `(∨ ,e0 ,e1)]
     [(∧ ,[e0] ,[e1]) `(∧ ,e0 ,e1)]))

(define-pass remove-same-twice-from-clause : K-canonical (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,e* ...) `(∨ ,(remove-duplicates e*) ...)]))

(define-pass eliminate-bottom : K-canonical (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,e* ...)
      (define new-e* (remove* '(⊥) e*))
      (cond
        [(empty? new-e*) '⊥]
        [(= (length new-e*) 1) (first new-e*)]
        [else `(∨ ,new-e* ...)])]))

(define-pass neg : K-canonical (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(¬ ,e) e]
     [else `(¬ ,e)]))
(define-pass offset-literal-and-its-negation : K-canonical (e) -> K-canonical ()
  (T : Expr (e) -> Expr ()
     [(∨ ,e* ...)
      (define new-e* (remove* (map neg e*) e*))
      (if (empty? new-e*)
          '⊤
          `(∨ ,new-e* ...))]))

(define K->canonical
  (compose offset-literal-and-its-negation
           eliminate-bottom
           remove-same-twice-from-clause
           clausal-fuse-clause
           K->clausal
           parse-K))

(module+ test
  (require rackunit)

  (define helper-K->canonical (compose unparse-K-canonical
                                       K->canonical))

  (check-equal? (helper-K->canonical '(∨ (¬ (∨ (¬ X) Y))
                                         (∨ (¬ Y) Z)))
                '(∧ (∨ X (¬ Y) Z) (∨ (¬ Y) Z)))

  (check-equal? (helper-K->canonical '(∨ C (∨ ⊤ (∨ A B))))
                '⊤)

  (check-equal? (helper-K->canonical '(∨ A (¬ A)))
                '⊤)

  (check-equal? (helper-K->canonical '(∨ ⊥ A))
                'A))
