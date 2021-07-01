#lang rosette

(require rosette/solver/smt/z3)
(require "datatypes.rkt")
(require (file "gf2^8.rkt"))
(require "aes-constants.rkt")

;; Solve

(current-solver (z3 #:logic 'QF_BV))

(define-symbolic x0 x1 x2 x3 x4 u8?)

;; (define sol
;;   (solve (assert (eq? (sbox x0) (u8 #x63)))))
;; (evaluate x0 sol)

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
  (gf+ (sbox (gf* t e)) f))
(define (get-key-2 e t f)
  (gf+ (sbox (gf*2 (gf* t e))) f))
(define (get-key-3 e t f)
  (gf+ (sbox (gf*3 (gf* t e))) f))

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

(exploit f0 e0)
(println "//")
(exploit f1 e1)
(println "//")
(exploit f2 e2)

