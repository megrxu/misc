#lang rosette/safe

(require rosette/lib/synthax)
(require rosette/solver/smt/z3)
(require "datatypes.rkt")

;; Element datatype

(define one (u4 1))
(define zero (u4 0))

(current-solver (z3 #:logic 'QF_BV ))
;; (current-bitwidth #f)

;; Element datatype

(define (gf* a b)
  (define (iter a b p)
    (if (or (bvzero? a) (bvzero? b)) p
        (let ([p_ (if (bvzero? (lsb b)) p
                      (bvxor a p))]
              [a_ (if (bvzero? (msb a))
                      (bvshl a one)
                      (bvxor (u4 #x3)
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
  (gf^n a (u4 14)))

(define (gf*2 x)
  (bvxor (bvshl x (u4 1))
         (bvlshr (bvashr x (u4 4)) (u4 2))))

(define (gf*3 x)
  (bvxor (bvxor (bvxor x (u4 2)) (bvadd x x))
         (bvlshr (bvnot (u4 #xd)) (bvlshr x (u4 3)))))

(define (gf*4 x)
  (bvxor (bvand (bvnot (u4 9)) (bvashr x (u4 1)))
         (bvor (bvlshr x (u4 2)) (bvshl x (u4 2)))))

(define (gf*5 x)
  (bvxor (gf*4 x) x))

(define (gf*6 x)
  (gf*2 (gf*3 x)))

(define (gf*7 x)
  (bvxor (gf*3 x) (gf*4 x)))

(define (gf*14 x)
  (gf*2 (gf*7 x)))

;; Verify the fast impl is correct

(define-symbolic x y u4?)

(current-solver (z3))
(verify (assert (forall (list x) (eq? (gf* x (u4 2)) (gf*2 x)))))
(verify (assert (forall (list x) (eq? (gf* x (u4 3)) (gf*3 x)))))
(verify (assert (forall (list x) (eq? (gf* x (u4 4)) (gf*4 x)))))
(verify (assert (forall (list x) (eq? (gf* x (u4 5)) (gf*5 x)))))
(verify (assert (forall (list x) (eq? (gf* x (u4 6)) (gf*6 x)))))
(verify (assert (forall (list x) (eq? (gf* x (u4 7)) (gf*7 x)))))
;; (verify (assert (forall (list x) (eq? (gf* x (gf*14 x)) one))))

(define sol
  (solve (assert (eq? (gf* x (u4 5)) (u4 1)))))
(evaluate x sol)
(evaluate (gf* x (u4 5)) sol)

(current-solver (z3 #:logic 'QF_BV))

;; Used to generate fast unary functions for bit vectors

;; (define-grammar (unary-u4 x)
;;   [expr (choose x (?? u4?)
;;                 ((bop) (expr) (expr))
;;                 ((uop) (expr)))]
;;   [bop  (choose bvadd bvsub bvand
;;                 bvxor bvor  bvshl
;;                 bvlshr bvashr)]
;;   [uop  (choose bvneg bvnot)])

;; (define (fast-unary x)
;;   (unary-u4 x #:depth 4))

;; (define (tpl x)
;;   (gf* x (u4 7)))

;; (define sol
;;    (synthesize
;;     #:forall    (list x)
;;     #:guarantee (assert (eq? (fast-unary x) (tpl x)))))

;; (print-forms sol)

;(evaluate (fast-unary one) sol)
;(tpl one)
