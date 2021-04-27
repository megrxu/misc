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

;; (define-symbolic t u8?)
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

(define (rcon n)
  (gf^n (u8 2) (u8 n)))

; Test mat-mul

(define mat-mix^-1
  (make-state
   '((#x0e #x09 #x0d #x0b)
     (#x0b #x0e #x09 #x0d)
     (#x0d #x0b #x0e #x09)
     (#x09 #x0d #x0b #x0e))))

(define mat-mix
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

(define (map2 op l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (op (car l1) (car l2))
            (map2 op (cdr l1) (cdr l2)))))

(define (mat-map op m)
  (map (lambda (col) (map op col)) m))

(define (mat-map2 op m n)
  (map2 (lambda (a b) (map2 op a b)) m n))

(define (mix-columns m)
  (mat* m mat-mix))

(define (mix-columns^-1 m)
  (mat* m mat-mix^-1))

(define (subbytes m)
  (mat-map sbox m))

(define (subbytes^-1 m)
  (mat-map sbox^-1 m))

(define (shift-rows m)
  (let ([e0 (car m)]
        [e1 (cadr m)]
        [e2 (caddr m)]
        [e3 (cadddr m)])
    (list (list (car e0) (cadr e1) (caddr e2) (cadddr e3))
          (list (car e1) (cadr e2) (caddr e3) (cadddr e0))
          (list (car e2) (cadr e3) (caddr e0) (cadddr e1))
          (list (car e3) (cadr e0) (caddr e1) (cadddr e2)))))

(define (shift-rows^-1 m)
  (let ([e0 (car m)]
        [e1 (cadr m)]
        [e2 (caddr m)]
        [e3 (cadddr m)])
    (list (list (car e0) (cadr e3) (caddr e2) (cadddr e1))
          (list (car e1) (cadr e0) (caddr e3) (cadddr e2))
          (list (car e2) (cadr e1) (caddr e0) (cadddr e3))
          (list (car e3) (cadr e2) (caddr e1) (cadddr e0)))))

(define (add-key m k)
  (mat-map2 gf+ m k))

(assert (eq? (shift-rows (shift-rows^-1 mat-mix)) mat-mix))
(assert (eq? (subbytes (subbytes^-1 mat-mix)) mat-mix))
(assert (eq? (add-key (add-key mat-mix mat-mix^-1) mat-mix^-1) mat-mix))

(provide u8 u8? zero one make-state
         mat-map mat-map2 map2
         add-key
         shift-rows shift-rows^-1
         subbytes subbytes^-1
         mix-columns mix-columns^-1
         sbox sbox^-1
         mat-id mat-mix mat-mix^-1)

(rcon 0)
