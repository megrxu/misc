#lang rosette/safe

(require "datatypes.rkt")

;; Element datatype

(define one (u8 1))
(define zero (u8 0))

;; GF2pow8 Operations

(define (gf+ a b)
  (bvxor a b))

(define (gf* a b)
  (define (iter a b p)
    (if (or (bvzero? a) (bvzero? b)) p
        (let ([p_ (if (bvzero? (lsb b)) p
                      (bvxor a p))]
              [a_ (if (bvzero? (msb a))
                      (bvshl a one)
                      (bvxor (u8 #x1b)
                             (bvshl a one)))]
              [b_ (bvlshr b one)])
          (iter a_ b_ p_))))
  (iter a b zero))

(define (gf^n a n)
  (define (iter acc a n)
    (cond [(bvzero? n) acc]
          [(bvzero? (lsb n)) (iter acc (gf* a a) (bvlshr n one))]
          [else (iter (gf* acc a) (gf* a a) (bvlshr n one))]))
  (iter one a n))

(define (gf^-1 a)
  (gf^n a (u8 254)))

(provide (all-defined-out))
