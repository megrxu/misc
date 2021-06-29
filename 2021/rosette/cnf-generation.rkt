#lang rosette/safe

(define (new-var)
  (define-symbolic* b boolean?)
  b)

(verify (assert #t))

(assert (let ([x (new-var)])
          (forall (list x)(eq? x #t))))


