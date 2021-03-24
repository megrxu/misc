#lang rosette/safe
(require rosette/lib/synthax)

;; Element datatype

(define u8? (bitvector 8))
(define (u8 i) (bv i 8))

(define one (u8 1))
(define zero (u8 0))

;; State Mk

(define (make-column l)
  (map u8 l))

(define (make-state l)
  (map make-column l))

;; GF2pow8 Operations

(define (gf+ a b)
  (bvxor a b))

(define (gf* a b)
  (define (iter a b p)
    (cond
      [(or (bvzero? a) (bvzero? b)) p]
      [else (let ([p_ (if (bvzero? (lsb b))
                          p
                          (bvxor a p))])
              (if (bvzero? (msb a))
                  (iter (bvshl a one) (bvlshr b one) p_)
                  (iter (bvxor (u8 #x1b)
                               (bvshl a one))
                        (bvlshr b one) p_)))]))
  (iter a b zero))

(define (gf^n a n)
  (define (iter acc a n)
    (cond [(bvzero? n) acc]
          [(bvzero? (lsb n)) (iter acc (gf* a a) (bvlshr n one))]
          [else (iter (gf* acc a) (gf* a a) (bvlshr n one))]))
  (iter one a n))

(define (gf^-1 a)
  (gf^n a (u8 254)))

(define-grammar (fast-u8 x)
  [expr
   (choose x (?? u8?)
           ((bop) (expr) (expr))
           ((uop) (expr)))]
  [bop
   (choose bvadd bvsub bvand bvor bvxor bvshl bvlshr bvashr)]
  [uop
   (choose bvneg bvnot)])

;; Sbox
(define (affine q l)
  (foldl bvxor zero
         (map (lambda (x) (bvrol q (u8 x))) l)))
(define (sbox i)
  (let [(b (gf^-1 i))]
    (bvxor (affine b '(0 1 2 3 4)) (u8 #x63))))

(define (sbox^-1 i)
  (let [(s (bvxor (affine i '(1 3 6)) (u8 #x05)))]
    (gf^-1 s)))

;; (assert (eq? (gf* t (gf^-1 t)) (u8 1)))
;; Quite slow

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

; Test mat-mul

(define mat-mix
  (make-state
   '((#x0e #x09 #x0d #x0b)
     (#x0b #x0e #x09 #x0d)
     (#x0d #x0b #x0e #x09)
     (#x09 #x0d #x0b #x0e))))

(define mat-mix^-1
  (make-state
   '((#x02 #x01 #x01 #x03)
     (#x03 #x02 #x01 #x01)
     (#x01 #x03 #x02 #x01)
     (#x01 #x01 #x03 #x02))))

(define mat-id
  (make-state
   '((#x01 #x00 #x00 #x00)
     (#x00 #x01 #x00 #x00)
     (#x00 #x00 #x01 #x00)
     (#x00 #x00 #x00 #x01))))

(assert (eq? (mat* mat-mix mat-mix^-1) mat-id))
(assert (eq? (mat* mat-mix^-1 mat-mix) mat-id))

;; Round Operations

(define (mat-map op m)
  (map (lambda (col) (map op col)) m))

(define (mix-column m)
  (mat* m mat-mix))

(define (mix-column^-1 m)
  (mat* m mat-mix^-1))

(define (subbytes m)
  (mat-map sbox m))

(define (subbytes^-1 m)
  (mat-map sbox^-1 m))

(define (lrol l r)
  (define (iter i res)
    (cond [(eq? i 0) res]
          [else (iter (- i 1) (append (cdr res) (list (car res))))]))
  (iter r l))

;; (define (shift-rows m)
;;   (transpose (for/lists (acc)
;;                         ([c (transpose m)]
;;                          [r '(0 1 2 3)])
;;                (lrol c r))))

;; (define (shift-rows^-1 m)
;;   (transpose (for/lists (acc)
;;                         ([c (transpose m)]
;;                          [r '(0 3 2 1)])
;;                (lrol c r))))

;; (define (add-key m k)
;;   (for/lists (acc)
;;              ([mv m]
;;               [kv k])
;;     (for/lists (accv)
;;                ([me mv]
;;                 [ke kv])
;;       (gf+ me ke))))

;; Note: for/lists is not safe. It operates on symbolic values in current design.

;; (assert (eq? (shift-rows (shift-rows^-1 mat-mix)) mat-mix))
(assert (eq? (subbytes (subbytes^-1 mat-mix)) mat-mix))
