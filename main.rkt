#lang racket
(require "propositional-logic.rkt"
         "clausal-canonical-form.rkt")

(define K->cnf (compose canonical->cnf
                        clausal->canonical
                        K->clausal
                        parse-K))

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
             (eq? (cdr left) (cdr right)))
        (set-subtract rs (set left right))
        rs)))
(define (resolution kb-rules query)
  (define new (set))
  (let/ec return
    (let loop ([kb (K->cnf `(∧ (¬ ,query) ,kb-rules))])
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

(module+ main
  (resolution '(∧
                (∧ (∨ (¬ A) B)
                   (∨ (¬ B) C))
                A)
              'C)

  (resolution '(¬ A)
              'A)
  )
