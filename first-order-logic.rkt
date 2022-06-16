#lang racket/base
(provide KF
         parse-KF
         unparse-KF)
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

(define-parser parse-KF KF)
