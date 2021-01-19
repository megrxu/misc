#lang racket

;; Naive Hoare Logic in Lisp

;; - https://bor0.wordpress.com/2021/01/18/towards-hoare-logic-for-a-small-imperative-language-in-haskell/

; arithmetic expressions
(define (aeval ctx aexp)
  (match aexp
    [(list 'id  n) (hash-ref ctx n)]
    [(list 'int n) n]
    [(list 'add m n) (+ (aeval ctx m) (aeval ctx n))]
    [(list 'sub m n) (- (aeval ctx m) (aeval ctx n))]
    [(list 'mul m n) (* (aeval ctx m) (aeval ctx n))]))

; boolean expressions
(define (beval ctx bexp)
  (match bexp
    [(list 'b true) true]
    [(list 'b false) false]
    [(list 'not bexp) (not (beval ctx bexp))]
    [(list 'and bexp bexp) (and (beval ctx bexp) (beval ctx bexp))]
    [(list 'or bexp bexp) (or (beval ctx bexp) (beval ctx bexp))]
    [(list '= e1 e2) (= (aeval ctx e1) (aeval ctx e2))]
    [(list '> e1 e2) (> (aeval ctx e1) (aeval ctx e2))]
    [(list '< e1 e2) (< (aeval ctx e1) (aeval ctx e2))]))

(define (ceval ctx stmt)
  (match stmt
    [(list 'skip) (ctx)]
    [(list 'seq s1 s2) (ceval (ceval ctx s1) s2)]
    [(list 'ifelse bexp s1 s2) (if (beval ctx bexp)
                                   (ceval ctx s1)
                                   (ceval ctx s2))]
    [(list 'while bexp s) (if (beval ctx bexp)
                              (ceval (ceval ctx s)
                                    (list 'while bexp s))
                              ctx)]
    [(list 'assign name aexp) (hash-set ctx name (aeval ctx aexp))]))

;; Compute the factorial number of variable x
;; Z := X
;; Y := 1
;; while (~Z = 0)
;;   Y := Y * Z
;;   Z := Z - 1

(define fact_prog
  (let* ([l1 (list 'assign "z" (list 'id "x"))]
         [l2 (list 'assign "y" (list 'int 1))]
         [l4 (list 'assign "y" (list 'mul (list 'id "y") (list 'id "z")))]
         [l5 (list 'assign "z" (list 'sub (list 'id "z") (list 'int 1)))]
         [l3 (list 'while (list 'not (list '= (list 'id "z") (list 'int 0)))
                   (list 'seq l4 l5))])
    (list 'seq l1 (list 'seq l2 l3))))

(define init
  (hash-set (hash) "x" 5))

(ceval init fact_prog)

;; A simple substitution
(define (sassign q aexp id)
  (match q
    [(list '= (list 'id aid) b) #:when (eq? aid id) (list '= b aexp)]
    [(list '= a (list 'id bid)) #:when (eq? bid id) (list '= a aexp)]))

(define (hassign c q)
  (match c
    [(list 'assign id aexp) (list 'triple (sassign q aexp id) c q)]))

(define (triple? tri)
  (match tri
    [(list 'triple p c q) #t]
    [else #f]))

(define (hseq t1 t2)
  (if (and (triple? t1) (triple? t2) (equal? (cadddr t1) (cadr t2)))
      (list 'triple (cadr t1) (list 'seq (caddr t1) (caddr t2)) (cadddr t2))
      #f))

(define (hskip c q)
  (list 'triple q c q))

(define after-assign
  (hassign (list 'assign "x" (list 'int 3))
           (list '= (list 'id "x") (list 'int 3))))
(define after-skip
  (hskip (list 'skip)
         (list '= (list 'id "x") (list 'int 3))))

(hseq after-assign after-skip)
