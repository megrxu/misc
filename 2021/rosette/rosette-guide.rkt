#lang rosette/safe

(require rosette/lib/synthax)

;; Symbolics

(define-symbolic b boolean?)
(define-symbolic* n integer?)

(eq? n n)
(boolean? b)

(define (static) (define-symbolic x boolean?) x)
(define (dynamic) (define-symbolic* y boolean?) y)

(eq? (dynamic) (dynamic))

;; VC Asserts

(vc-asserts (vc))
(assert (not b))
(vc)
(clear-vc!)
(vc)

;; Assumes

(assume #t)
(vc-assumes (vc))
(clear-vc!)

(define-symbolic i j integer?)
(assume (> j 0))
(vc-assumes (vc))
(assert (< (- i j) i))
(vc-asserts (vc))
(vc)

;; Bitvectors

(define int32? (bitvector 32))

(define (int32 i)
  (bv i int32?))

(int32? 1)
(int32? (int32 90))
(int32 1)

(define (bvmid lo hi)
  (bvsdiv (bvadd lo hi) (int32 2)))

(define (check-mid impl lo hi)     ; Assuming that
  (assume (bvsle (int32 0) lo))    ; 0 ≤ lo and
  (assume (bvsle lo hi))           ; lo ≤ hi,
  (define mi (impl lo hi))         ; and letting mi = impl(lo, hi) and
  (define diff                     ; diff = (hi - mi) - (mi - lo),
    (bvsub (bvsub hi mi)
           (bvsub mi lo)))         ; we require that
  (assert (bvsle lo mi))           ; lo ≤ mi,
  (assert (bvsle mi hi))           ; mi ≤ hi,
  (assert (bvsle (int32 0) diff))  ; 0 ≤ diff, and
  (assert (bvsle diff (int32 1)))) ; diff ≤ 1.

(check-mid bvmid (int32 10) (int32 10000))

;; Verify

(define-symbolic l h int32?)
(define cex (verify (check-mid bvmid l h)))
(print-forms cex)

(define cl (evaluate l cex))
(define ch (evaluate h cex))

(define (bvmid-no-of lo hi)
  (bvadd lo (bvsdiv (bvsub hi lo) (int32 2))))
;; (verify (check-mid bvmid-no-of l h)) ;; (unsat) means no counter examples

;; Synthesis

(define-grammar (fast-int32 x y)
  [expr
   (choose x y (?? int32?)
           ((bop) (expr) (expr))
           ((uop) (expr)))]
  [bop
   (choose bvadd bvsub bvand bvor bvxor bvshl bvlshr bvashr)]
  [uop
   (choose bvneg bvnot)])

(define (bvmid-fast lo hi)
  (fast-int32 lo hi #:depth 2))

;; (define sol
;;     (synthesize
;;      #:forall    (list l h)
;;      #:guarantee (check-mid bvmid-fast l h)))
;; (print-forms sol)


;; Angelic Execution

(define (bvmid-impl lo hi)
    (bvlshr (bvadd hi lo) (bv 1 32)))

(define sol
  (solve
   (begin
     (assume (not (equal? l h)))
     (assume (bvsle (int32 0) l))
     (assume (bvsle l h))
     (assert (equal? (bvand l h) (bvmid-impl l h))))))

(evaluate (bvand l h) sol)
(evaluate (bvmid-impl l h) sol)

;; Fun Exercise

(define (bvmid-and? lo hi)
  (equal? (fast-int32 lo hi #:depth 1) (fast-int32 lo hi #:depth 1)))

(print-forms
   (synthesize
    #:forall (list l h)
    #:guarantee
    (begin
      (assume (not (equal? l h)))
      (assume (bvsle (int32 0) l))
      (assume (bvsle l h))
      (assert
       (<=> (bvmid-and? l h)
            (equal? (bvand l h) (bvmid-fast l h)))))))

