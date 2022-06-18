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
    (define not-q (logic->cnf `(¬ ,query)))
    (let/ec return
      (let loop ([kb (apply set-union (map logic->cnf kb-rules))])
        (for ([c (in-combinations (append (set->list kb) (set->list not-q)) 2)])
          (define resolvents (resolve (first c) (second c)))
          (if (set-empty? resolvents)
              (return #t)
              (begin
                (set! new
                      (set-add new
                               resolvents)
                      ))))
        (if (subset? new kb)
            (return #f)
            (begin
              (loop (set-union kb new)))))))
  resolution)

(module+ main
  (define resolution-K (make-resolution K->cnf eq?))
  (resolution-K '((→ A B)
                  (→ B C)
                  A)
                'C)
  (resolution-K '((¬ A))
                'A)

  (define resolution-KF (make-resolution KF->cnf eq?))
  (resolution-KF '((∀ (a) (→ (Red a)
                             (Sweet a)))
                   (Red Apple))
                 '(Sweet Apple))
  )
