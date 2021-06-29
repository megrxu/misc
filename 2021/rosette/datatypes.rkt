#lang rosette/safe

(define u8? (bitvector 8))
(define u4? (bitvector 4))
(define (u8 i) (bv i 8))
(define (u4 i) (bv i 4))

(provide (all-defined-out))
