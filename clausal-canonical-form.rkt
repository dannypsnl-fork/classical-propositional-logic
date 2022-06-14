#lang nanopass
(provide K->clausal
         clausal->canonical
         canonical->cnf)

(require "classical-logic.rkt")

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
  (let loop ([e-s e])
    (define e-t (T e-s))
    (if (equal? (unparse-K e-t) (unparse-K e-s))
        e-t
        (loop e-t))))

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

(define clausal->canonical
  (compose offset-literal-and-its-negation
           eliminate-bottom
           remove-same-twice-from-clause
           clausal-fuse-clause))

(define-pass canonical->cnf : K-canonical (e) -> * ()
  (Pos : Expr (e) -> * ()
       [,x (set (cons #t x))]
       [,⊤ (set)]
       [,⊥ (set (set))]
       [(→ ,e0 ,e1) (set-union (Neg e0) (Pos e1))]
       [(∧ ,e* ...) (apply set (map Pos e*))]
       [(∨ ,e* ...) (apply set-union (map Pos e*))]
       [(¬ ,e) (Neg e)])
  (Neg : Expr (e) -> * ()
       [,x (set (cons #f x))]
       [,⊤ (set (set))]
       [,⊥ (set)]
       [(→ ,e0 ,e1) (set (Pos e0) (Neg e1))]
       [(∧ ,e* ...) (apply set-union (map Neg e*))]
       [(∨ ,e* ...) (apply set (map Neg e*))]
       [(¬ ,e) (Pos e)])
  (Pos e))

(module+ test
  (require rackunit)

  (define helper-K->canonical (compose unparse-K-canonical
                                       clausal->canonical
                                       K->clausal
                                       parse-K))

  (check-equal? (helper-K->canonical '(∨ (¬ (∨ (¬ X) Y))
                                         (∨ (¬ Y) Z)))
                '(∧ (∨ X (¬ Y) Z) (∨ (¬ Y) Z)))

  (check-equal? (helper-K->canonical '(∨ C (∨ ⊤ (∨ A B))))
                '⊤)

  (check-equal? (helper-K->canonical '(∨ A (¬ A)))
                '⊤)

  (check-equal? (helper-K->canonical '(∨ ⊥ A))
                'A))
