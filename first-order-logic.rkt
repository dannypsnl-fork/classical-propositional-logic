#lang racket/base
(provide KF)
(require nanopass
         "propositional-logic.rkt")

(define-language KF
  (extends K)
  (Expr (e)
        (- x)
        (+ t #| term |#
           (∀ (x* ...) e) #| forall |#
           (∃ (x* ...) e) #| exists |#))
  (Term (t)
        (+ x #| variable |#
           (x t* ...) #| predicate |#)))
