#lang rosette/safe

(require rosette/lib/synthax)
(require rosette/solver/smt/z3)
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

(define (gf*2 x)
  (bvxor (bvadd x x)
         (bvand (u8 #x1b)
                (bvashr x (u8 #x0a)))))

(define (gf*3 x)
  (bvxor (bvxor x (bvadd x x))
         (bvand (bvneg (u8 #xe5))
                (bvashr x (u8 #x3c)))))

(current-solver (z3 #:logic 'QF_BV))

(define-symbolic x y u8?)

;; Used to generate fast unary functions for bit vectors
;; (define-grammar (unary-u8 x)
;;   [expr (choose x (?? u8?)
;;                 ((bop) (expr) (expr))
;;                 ((uop) (expr)))]
;;   [bop  (choose bvadd bvsub bvand
;;                 bvxor bvor  bvshl
;;                 bvlshr bvashr)]
;;   [uop  (choose bvneg bvnot)])

;; (define (fast-unary x)
;;   (unary-u8 x #:depth 3))

;; (define (affine q l)
;;   (foldl bvxor zero
;;          (map (lambda (x) (bvrol q (u8 x))) l)))

;; (define (tpl x)
;;   (affine x '(1 2 3 4 5)))

;; (define sol
;;    (synthesize
;;     #:forall    (list x)
;;     #:guarantee (assert (eq? (fast-unary x) (tpl x)))))

;; (print-forms sol)

;(evaluate (fast-unary one) sol)
;(tpl one)

;; (current-solver (z3))
;; (verify (assert (forall (list x) (eq? (gf* x (u8 3)) (gf*3 x)))))

(provide (all-defined-out))
