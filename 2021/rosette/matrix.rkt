#lang rosette/safe

(require (file "gf2^8.rkt"))

;; Matrix Multiplication

(define (dot-product v w)
  (foldl gf+ zero (map gf* v w)))

(define (transpose mat)
    (apply map list mat))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (x) (matrix-*-vector cols x)) m)))
(define mat* matrix-*-matrix)

(provide (all-defined-out))
