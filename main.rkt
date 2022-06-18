#lang racket
(require "K-cnf.rkt"
         "KF-cnf.rkt")

(define (make-resolution logic->cnf unifier)
  (define (resolve r1 r2)
    (define resolvents (set-union r1 r2))
    (for/fold ([rs resolvents])
              ([product (in-combinations (set->list resolvents) 2)])
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
              (return 'contradiction)
              (begin
                (set! new
                      (set-add new
                               resolvents)
                      ))))
        (if (subset? new kb)
            (return 'correct)
            (begin
              (loop (set-union kb new)))))))
  resolution)

(module+ main
  (define resolution-K (make-resolution K->cnf eq?))
  ; FIXME: expected correct
  (resolution-K '(∧
                  (∧ (→ A B)
                     (→ B C))
                  A)
                'C)
  (resolution-K '(¬ A)
                'A)

  (define resolution-KF (make-resolution KF->cnf eq?))
  ; (resolution-KF '(∧ (∀ (a) (→ (Red a)
  ;                              (Sweet a)))
  ;                    (Red Apple))
  ;                '(Sweet Apple))
  )
