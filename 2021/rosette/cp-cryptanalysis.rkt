#lang rosette/safe

(define-symbolic a b c d e boolean?)

(assume (= (length (filter (lambda (x) (eq? x #t))
                           (list a c d e))) 3))
(assume (not (eq? a b)))
(assume (|| (&& (eq? a #f) (eq? b #f) (eq? c #f))
            (&& (eq? a #f) (eq? b #t) (eq? c #f))
            (&& (eq? a #f) (eq? b #t) (eq? c #t))))

(evaluate (list a b c d e) (solve #t))
