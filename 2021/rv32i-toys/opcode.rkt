#lang racket

(require match-string)
(require rebellion/binary/bitstring)
(require rebellion/binary/immutable-bytes)

;; Aux Functions

(define (make-bits n)
  (compose (lambda (x) (take x n))
           sequence->list
           bytes->bitstring
           bytes->immutable-bytes
           (lambda (x)
             (integer->integer-bytes x 4 #f))))

;; Registers

(define (abi-idx reg)
  (define c string->number)
  (match (symbol->string reg)
    [(string-append "x" num) (c num)]
    [(string-append "t" num) (let [(n (c num 5))]
                               (if (n < 3)
                                   (+ 5 n)
                                   (+ 25 n)))]
    [(string-append "a" num) (+ 10 (c num))]
    [(string-append "s" num) (+ 16 (c num))]))

(define (reg-idx-num reg)
  (match reg
    ['zero 0]
    ['ra 1]
    ['sp 2]
    ['gp 3]
    ['tp 4]
    ['fp 8]
    ['s0 8]
    ['s1 9]
    [else (abi-idx reg)]))
(define (reg-idx reg)
  (make-bits 5)
  (reg-idx-num reg))

;; Dispatch Instruction Types

(define (r-type? op)
  (member op '(add sub xor or and sll srl sra slt sltu)))
(define (i-type? op)
  (member op '(addi xori ori andi slli srli srai slti sltui
                    lb lh lw lbu lhu)))
(define (s-type? op)
  (member op '(sw sb sh)))
(define (b-type? op)
  (member op '(beq bne blt bge bltu bgeu)))
(define (j-type? op)
  (member op '(jal jalr)))
(define (u-type? op)
  (member op '(lui)))

(define (j-cat? op)
  (member op '(jalr)))
(define (m-cat? op)
  (member op '(lb lh lw lbu lhu)))
(define (e-cat? op)
  (member op '(ecall ebreak)))

;; Opcodes

(define (get-opc op)
  (cond [(r-type? op) #b0110011]
        [(and (i-type? op) (j-cat? op)) #b1100111]
        [(and (i-type? op) (m-cat? op)) #b0000011]
        [(and (i-type? op) (e-cat? op)) #b1110011]
        [(i-type? op) #b0010011]
        [(b-type? op) #b1100011]
        [(s-type? op) #b0100011]
        ['lui         #b0110111]
        ['auipc       #b0010111]
        [else #b0000000]))

(define (get-fn3 op)
  (define in (lambda (x) (member op x)))
  (cond [(in '(add addi sub lb sb beq jalr ecall ebreak)) #x0]
        [(in '(sll slli lh sh bne)) #x1]
        [(in '(slt slti lw sw)) #x2]
        [(in '(sltu sltiu)) #x3]
        [(in '(xor xori lbu blt)) #x4]
        [(in '(srl srli srai lhu bge)) #x5]
        [(in '(or ori bltu)) #x6]
        [(in '(and andi bgeu)) #x7]
        [else #x0]))

(define (get-fn7 op)
  (match op
    ['sub    #x20]
    ['sra    #x20]
    ['srai   #x20]
    ['ebreak #x01]
    [else #x00]))

(define (get-ops op)
  (list ((make-bits 7) (get-opc op))
        ((make-bits 3) (get-fn3 op))
        ((make-bits 7) (get-fn7 op))))

(provide (all-defined-out))
