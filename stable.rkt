#lang racket/base
(provide stable)

(define (stable expression transformer unparser)
  (let loop ([e-s expression])
    (define e-t (transformer e-s))
    (if (equal? (unparser e-t) (unparser e-s))
        e-t
        (loop e-t))))
