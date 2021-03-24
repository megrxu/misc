#lang rosette/safe

(require "partial-aes.rkt")

(define ciphertext
  (make-state
   '((#x39 #x25 #x84 #x1d)
     (#x02 #xdc #x09 #xfb)
     (#xdc #x11 #x85 #x97)
     (#x19 #x6a #x0b #x32))))

(define r10
  (make-state
   '((#xd0 #x14 #xf9 #xa8)
     (#xc9 #xee #x25 #x89)
     (#xe1 #x3f #x0c #xc8)
     (#xb6 #x63 #x0c #xa6))))

(define r9
  (make-state
   '((#xac #x77 #x66 #xf3)
     (#x19 #xfa #xdc #x21)
     (#x28 #xd1 #x29 #x41)
     (#x57 #x5c #x00 #x6e))))

(define mid
  (make-state
   '((#x87 #x6e #x46 #xa6)
     (#xf2 #x4c #xe7 #x8c)
     (#x4d #x90 #x4a #xd8)
     (#x97 #xec #xc3 #x95))))

(let* ([s0 ciphertext]
       [s1 (add-key s0 r10)]
       [s2 (shift-rows^-1 s1)]
       [s3 (subbytes^-1 s2)]
       [s4 (add-key s3 r9)]
       [s5 (mix-columns^-1 s4)])
  (assert (eq? s5 mid)))
