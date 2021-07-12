#lang rosette

(require rosette/solver/smt/z3)
(require rosette/lib/synthax)
(require "datatypes.rkt")
(require (file "gf2^8.rkt"))
(require "aes-constants.rkt")

;; Solve

(current-solver (z3 #:logic 'QF_BV))

(define-symbolic x0 x1 x2 x3 x4 u8?)

(define (example f e)
  (let [(e0 (car f))
        (e1 (cadr f))
        (e2 (caddr f))
        (e3 (cadddr f))]
    (assert (eq? (sbox (gf+ x0 (gf*2 e))) (gf+ (sbox x0) e0)))
    (assert (eq? (sbox (gf+ x1 e))        (gf+ (sbox x1) e1)))
    (assert (eq? (sbox (gf+ x2 e))        (gf+ (sbox x2) e2)))
    (assert (eq? (sbox (gf+ x3 (gf*3 e))) (gf+ (sbox x3) e3)))))

;; To generate the diff mats
;; (let [(a (u8 #xeb))
;;       (b (u8 #x40))
;;       (c (u8 #xf2))
;;       (d (u8 #x1e))
;;       (e (u8 #xb3))]
;;   (println (gf+ (sbox a) (sbox (gf+ a (gf*2 e)))))
;;   (println (gf+ (sbox b) (sbox (gf+ b e))))
;;   (println (gf+ (sbox c) (sbox (gf+ c e))))
;;   (println (gf+ (sbox d) (sbox (gf+ d (gf*3 e))))))

(define-symbolic e0 e1 e2 u8?)
(define f0 (map u8 (list #xe7 #x51 #x47 #x99)))
(define f1 (map u8 (list #xca #x3b #xf4 #x85)))
(define f2 (map u8 (list #x79 #x04 #x0a #x02)))

;; Use the diff to get the error

(define sol
  (solve (begin
           (example f0 e0)  ;; e0 = 0x1e
           (example f1 e1)  ;; e1 = 0xe1
           (example f2 e2)  ;; e2 = 0xb3
           )))

(define-symbolic t k u8?)

(define (affine x)
  (bvxor (bvxor (bvrol x (u8 1))
                (bvrol x (u8 2)))
         (bvxor (bvrol x (u8 3))
                (bvxor x
                       (bvrol x (u8 4))))))

(define (affine^-1 x)
  (bvxor (bvrol x (u8 1))
         (bvxor (bvrol x (u8 3))
                (bvrol x (u8 6)))))

(define (make-theta-1 e)
  (lambda (de) (gf^-1 (gf* e (affine^-1 de)))))
(define (make-theta-2 e)
  (lambda (de) (gf^-1 (gf* (gf*2 e) (affine^-1 de)))))
(define (make-theta-3 e)
  (lambda (de) (gf^-1 (gf* (gf*3 e) (affine^-1 de)))))

;; Focus on one fault

(define (get-sol theta)
  (let [(sol (solve (assert (eq? (l t) theta))))]
    (let [(alpha (evaluate t sol))]
      (list alpha
            (bvxor alpha one)))))

(define (id a) a)
(define (get-key-1 e t f)
  (sbox (gf* t e)))
(define (get-key-2 e t f)
  (sbox (gf*2 (gf* t e))))
(define (get-key-3 e t f)
  (sbox (gf*3 (gf* t e))))

;; Use the error and the diff error to recover the key

(define (exploit f e)
  (let* [(e_ (evaluate e sol))
         (theta-1 (make-theta-1 e_))
         (theta-2 (make-theta-2 e_))
         (theta-3 (make-theta-3 e_))]
    (match f [(list fa fb fc fd)
              (let*-values [((a b c d) (values (theta-2 fa)
                                               (theta-1 fb)
                                               (theta-1 fc)
                                               (theta-3 fd)))
                            ((sa sb sc sd) (values (get-sol a)
                                                   (get-sol b)
                                                   (get-sol c)
                                                   (get-sol d)))]
                (values (map (lambda (t) (get-key-2 e_ t fa)) sa)
                        (map (lambda (t) (get-key-1 e_ t fb)) sb)
                        (map (lambda (t) (get-key-1 e_ t fc)) sc)
                        (map (lambda (t) (get-key-3 e_ t fd)) sd)))])))

;; Find the union set of key

(define-symbolic k0 k1 k2 k3 u8?)

(define (contains l b)
  (ormap (lambda (e) (eq? e b)) l))

(let-values
    [((kg00 kg01 kg02 kg03) (exploit f0 e0))
     ((kg10 kg11 kg12 kg13) (exploit f1 e1))
     ((kg20 kg21 kg22 kg23) (exploit f2 e2))]
  (let [(key (solve (begin (assert (&& (contains kg00 k0) (contains kg10 k0)))
                           (assert (&& (contains kg01 k1) (contains kg11 k1)))
                           (assert (&& (contains kg02 k2) (contains kg12 k2)))
                           (assert (&& (contains kg03 k3) (contains kg13 k3))))))]
    (evaluate (list k0 k1 k2 k3) key))
  )


(define-grammar (unary-u8 x)
  [expr (choose x
                ((bop) (expr) (expr))
                ((uop) (expr)))]
  [bop  (choose bvxor)]
  [uop  (choose id sbox sbox^-1 gf*2 gf*2 gf*3)])

(define (fast-unary x)
  (unary-u8 x #:depth 5))

(define get-key
   (solve (assert (&& (eq? (fast-unary (u8 #xe9)) (u8 #xd0))
                      (eq? (fast-unary (u8 #x09)) (u8 #x63))
                      (eq? (fast-unary (u8 #x89)) (u8 #x0c))
                      (eq? (fast-unary (u8 #x72)) (u8 #x89))))))

(print-forms get-key)
