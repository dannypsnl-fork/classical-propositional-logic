#lang racket
(require "propositional-logic.rkt"
         "K-cnf.rkt"
         "first-order-logic.rkt"
         "KF-cnf.rkt")

(define (make-resolution logic->cnf unifier)
  (define (resolve r1 r2)
    (define (->set r)
      (if (set? r) r (set r)))
    (define resolvents (set-union (->set r1) (->set r2)))
    (define (->list r)
      (if (set? r) (set->list r) (list r)))
    (for/fold ([rs resolvents])
              ([product (cartesian-product (->list r1) (->list r2))])
      (define left (first product))
      (define right (second product))
      (if (and (eq? (not (car left)) (car right))
               (unifier (cdr left) (cdr right)))
          (set-subtract rs (set left right))
          rs)))
  (define (resolution kb-rules query)
    (define new (set))
    (let/ec return
      (let loop ([kb (logic->cnf `(∧ (¬ ,query) ,kb-rules))])
        (for ([c (in-combinations (set->list kb) 2)])
          (define resolvents (resolve (first c) (second c)))
          (if (set-empty? resolvents)
              (return 'correct)
              (begin
                (set! new (set-union new resolvents)))))
        (if (subset? new kb)
            (return 'contradiction)
            (begin
              (loop (set-union kb new)))))))
  resolution)

(module+ main
  (define resolution-K (make-resolution K->cnf eq?))
  (resolution-K '(∧
                  (∧ (∨ (¬ A) B)
                     (∨ (¬ B) C))
                  A)
                'C)
  (resolution-K '(¬ A)
                'A)

  (define resolution-KF (make-resolution KF->cnf eq?))
  (resolution-KF '(∧ (∀ (a) (→ (Red a)
                               (Sweet a)))
                     (Red Apple))
                 '(Sweet Apple))
  )
