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
              (return (format "~a |- ~a" kb-rules query))
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

(define (occurs v t)
  (match t
    [`(,t* ...)
     (ormap (λ (t) (occurs v t)) t*)]
    [t (equal? v t)]))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (parameter? t2)
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (t2 t1)
                (error (format "~a occurs in ~a" (t2) t1)))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (andmap unify a* b*)]
    [(_ _) (eqv? t1 t2)]))

(module+ main
  (define resolution-K (make-resolution K->cnf eq?))
  (resolution-K '((→ A B)
                  (→ B C)
                  A)
                'C)
  (resolution-K '((¬ A))
                'A)

  (define resolution-KF (make-resolution KF->cnf unify))
  ;; FIXME: unification should correct this result
  (resolution-KF '((∀ (a) (→ (Red a)
                             (Sweet a)))
                   (Red Apple))
                 '(Sweet Apple))
  )
