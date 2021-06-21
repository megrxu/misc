#lang rosette/safe

(require rosette/lib/destruct)
(require rosette/lib/angelic)

(struct plus (a b) #:transparent)
(struct mul  (a b) #:transparent)
(struct square (m) #:transparent)

(define (interp p)
  (destruct p
            [(plus a b) (+ (interp a) (interp b))]
            [(mul  a b) (* (interp a) (interp b))]
            [(square m) (expt (interp m) 2)]
            [_ p]))

(define (??expr terminals)
  (define a (apply choose* terminals))
  (define b (apply choose* terminals))
  (choose* (plus a b)
           (mul a b)
           (square a)
           a))

(define-symbolic x a b integer?)  ; get access to more constants
(define sketch
  (??expr (list (??expr (list x a b)) (??expr (list x a b)) a)))

(define M
  (synthesize
    #:forall (list x)
    #:guarantee (assert (= (interp sketch) (interp (plus (square x) (mul x 10)))))))

(evaluate sketch M)
