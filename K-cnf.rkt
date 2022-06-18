#lang nanopass
(provide K->cnf)
(require "KF-cnf.rkt"
         "propositional-logic.rkt"
         "first-order-logic.rkt")

(define-pass K->KF : K (e) -> KF ()
  (T : Expr (e) -> Expr ()))

(define K->cnf (compose KF-internal->cnf
                        K->KF
                        parse-K))
