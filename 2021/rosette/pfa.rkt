#lang rosette/safe

(require rosette/lib/synthax)
(require "partial-aes.rkt")

(define (guess-key)
  (define-symbolic*
    k00 k01 k02 k03
    k10 k11 k12 k13
    k20 k21 k22 k23
    k30 k31 k32 k33 u8?)
  (list (list k00 k01 k02 k03)
        (list k10 k11 k12 k13)
        (list k20 k21 k22 k23)
        (list k30 k31 k32 k33)))

(define k10 (guess-key))
(define k09 (guess-key))

(define (contains-no m b)
  (not (ormap (lambda (col) (ormap (lambda (e) (eq? e b)) col)) m)))

;; When observe a new ciphertext, we can add new constraints.

(define (add-constraints ciphertext)
  (let* ([s0 ciphertext]
         [s1 (add-key s0 k10)]
         [s2 (shift-rows^-1 s1)]
         [s3 (subbytes^-1 s2)]
         [s4 (add-key s3 k09)]
         [s5 (mix-columns^-1 s4)])
    (and (contains-no s0 (u8 #x63)) (contains-no s5 (u8 #x63)))))
