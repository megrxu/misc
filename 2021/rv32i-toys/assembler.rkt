#lang racket

(require "opcode.rkt")

(define insts
  '(start
    (lw a0 zero 0)
    (lw a1 zero 4)
    loop
    (sub s1 zero a0)
    (sw s1 a1 0)
    (addi a0 a0 1)
    (beq a0 zero start)
    (jal loop)))

;; Get the Label Table

(define (label? i)
  (symbol? i))

(define (has-label? i)
  (and (list? i) (has-label-op? (car i))))
(define (has-label-op? op)
  (member op '(beq bne j jal jalr)))

(define (get-labels insts)
  (define (get-label-iter insts addr labels)
    (cond [(null? insts) labels]
          [(label? (car insts)) (let ([new-labels (hash-set labels (car insts) addr)]
                                      [new-insts (cdr insts)])
                                  (get-label-iter new-insts addr new-labels))]
          [else (let ([new-insts (cdr insts)]
                      [new-addr (+ addr 1)])
                  (get-label-iter new-insts new-addr labels))]))
  (get-label-iter insts 0 (hash)))

;; Convert Labels to Imm Numbers

(define (purify insts labels)
  (define (attach-addr insts labels)
    (define (iter insts addr res)
      (cond [(null? insts) res]
            [(label? (car insts)) (iter (cdr insts) addr res)]
            [else (iter (cdr insts) (+ addr 1) (cons (cons addr (car insts)) res))]))
    (reverse (iter insts 0 null)))
  (define addr-insts (attach-addr insts labels))
  (define (ref-and-diff-last h v d)
    (let [(last (car (reverse v)))
          (tail (cdr (reverse v)))]
      (reverse (cons (* 4 (- (hash-ref h last) d)) tail))))
  (map (lambda (i) (if (has-label? (cdr i))
                       (ref-and-diff-last labels (cdr i) (car i))
                       (cdr i))) addr-insts))

;; Tag Instructions for easy processing

(define (tag-insts insts)
  (define (tag-inst inst)
    (define op (car inst))
    (cond [(r-type? op) (cons 'r inst)]
          [(i-type? op) (cons 'i inst)]
          [(s-type? op) (cons 's inst)]
          [(b-type? op) (cons 'b inst)]
          [(j-type? op) (cons 'j inst)]
          [(u-type? op) (cons 'u inst)]))
  (map tag-inst insts))

;; Digitalize

(define (digitalize insts)
  (define idx reg-idx)
  (define (digitalize-single inst)
    (match inst
      [(list 'r op rd rs1 rs2)  (list 'r (get-ops op) (idx rd) (idx rs1) (idx rs2))]
      [(list 'i op rd rs imm)   (list 'i (get-ops op) (idx rd) (idx rs) imm)]
      [(list 's op rs1 rs2 imm) (list 's (get-ops op) (idx rs1) (idx rs2) imm)]
      [(list 'b op rs1 rs2 imm) (list 'b (get-ops op) (idx rs1) (idx rs2) imm)]
      [(list 'j 'jal imm)       (list 'j (get-ops 'jal) (idx 'ra) imm)]
      [(list 'j op rd imm)      (list 'j (get-ops op) (idx rd) imm)]
      [(list 'u op rd imm)      (list 'u (get-ops op) (idx rd) imm)]
      [error "Invalid Instruction"]))
  (map digitalize-single insts))

;; Make 32-bit machine codes

(define (to-machine-code inst)
  (match-let
      ([(list op (list opc fn3 fn7) args ...) inst])
    (match (cons op args)
      [(list 'r rd rs1 rs2)
       (list fn7 rs2 rs1 fn3 rd opc)]
      [(list 'i rd rs imm)
       (list imm rs fn3 rd opc)]
      [(list 's rs1 rs2 imm)
       (list imm rs2 rs1 fn3 imm opc)]
      [(list 'b rs1 rs2 imm)
       (list imm rs2 rs1 fn3 imm opc)]
      [(list 'u rd imm)
       (list imm rd opc)]
      [(list 'j rd imm)
       (list imm rd opc)])))

(define (assemble insts)
  (let* [(labels (get-labels insts))
         (pure-insts (purify insts labels))
         (tagged-insts (tag-insts pure-insts))
         (tagged-digits (digitalize tagged-insts))]
    (map to-machine-code tagged-digits)))

(assemble insts)
