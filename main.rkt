#lang racket
(require "classical-logic.rkt"
         "clausal-canonical-form.rkt")

(module+ main
  (define t '(→ (∨ A B) (¬ (∧ (¬ A) (¬ B)))))
  (define t2 '(∨ (¬ (∨ (¬ X) Y))
                 (∨ (¬ Y) Z)))

  (define final (compose canonical->cnf
                         clausal->canonical
                         K->clausal
                         parse-K))

  (final t)
  (final t2)
  (final '(∨ A (¬ A)))
  (final '(∨ (¬ A) (¬ A))))
