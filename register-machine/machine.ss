#lang sicp

;;; exports
(#%require racket/base)
(provide (all-defined-out))

;;; utils
(define (tagged-list? x sym)
  (equal? sym (car x)))

;;; the general machine constructor
(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) 
                 register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;; the register constructor
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (set! contents value)))
            (else
             (error "Unknown request: 
                     REGISTER"
                    message))))
    dispatch))

;;; register shortcut functions
(define (get-contents reg)
  (reg 'get))
(define (set-contents! reg value)
  ((reg 'set) value))

;;; the stack constructor
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (top)
      (if (null? s)
          '()
          (car s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'top) (top))
            ((eq? message 'initialize)
             (initialize))
            (else 
             (error "Unknown request: STACK"
                    message))))
    dispatch))

;;; a customize machine constructor
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list 
            (list 'initialize-stack (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) 
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons 
                   (list name (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc 
                  (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) 
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) 
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

;;; machine shortcut functions
(define (start machine)
  (machine 'start))

(define (get-register-contents 
         machine register-name)
  (get-contents 
   (get-register machine register-name)))

(define (set-register-contents! 
         machine register-name value)
  (set-contents! 
   (get-register machine register-name) 
   value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;; the assembler ;;;;;;;;;;;;;;;;;;;;

;;; Assemble controller text into instructions, and create
;;; a label table to associate each label with corresponding
;;; instructions.
;;; Each instruction is a pair of <text> and <procedure>.
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

;;; Create an initial instructions from text and a label table.
;;; A *receive* function is used to update the instructions
;;; to add the appropriate procedure.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Duplicated label: ASSEMBLE" next-inst)
                   (receive 
                       insts
                       (cons 
                        (make-label-entry next-inst insts)
                        labels)))
               (receive
                   (cons (make-instruction next-inst) insts)
                   labels)))))))

;;; update initial instructions to add appropriate procedures
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) 
         labels
         machine
         pc
         flag
         stack
         ops)))
     insts)))

;;; the label entry constructor
(define (make-label-entry label-name insts)
  (cons label-name insts))

;;; labels selector
(define (lookup-label labels label-name)
  (if (null? labels)
      (error "Undefined label: ASSEMBLE" label-name)
      (if (equal? (caar labels) label-name)
          (cdar labels)
          (lookup-label (cdr labels) label-name))))

;;; the instruction constructor
(define (make-instruction text)
  (cons text '()))

;;; the instruction selectors and mutators
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))

;;; make appropriate procedure according to the instruction type
;;; same as *eval*
(define (make-execution-procedure inst-text labels machine pc flag stack ops)
  ;; (set-contents! pc (cdr (get-contents pc)))
  (cond ((eq? (car inst-text) 'assign)
         (make-assign
          inst-text machine labels ops pc))
        ((eq? (car inst-text) 'test)
         (make-test
          inst-text machine labels ops flag pc))
        ((eq? (car inst-text) 'branch)
         (make-branch
          inst-text machine labels ops flag pc))
        ((eq? (car inst-text) 'goto)
         (make-goto
          inst-text machine labels ops flag pc))
        (else
         (error "Unknown instruction type: ASSEMBLE" inst-text))))

;;; make procedure for assignment instruction
(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                ops)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()                      ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

;; make procedure for test instruction
(define (make-test inst machine labels ops flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                ops)))
          (lambda ()
            (set-contents!
             flag
             (condition-proc))
            (advance-pc pc)))
        (error "Bad Test instruction: ASSEMBLE" inst))))

;; make procedure for branch instruction
(define (make-branch inst machine labels ops flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad Branch instruction: ASSEMBLE" inst))))

;; make procedure for goto instruction
(define (make-goto inst machine labels ops flag pc)
  (let ((dest (goto-dest inst)))
    (cond
     ((label-exp? dest)
      (let ((insts
             (lookup-label labels
                           (label-exp-label dest))))
        (lambda ()
          (set-contents! pc insts))))
     ((register-exp? dest)
      (let ((reg
             (get-register
               machine (register-exp-reg dest))))
        (lambda ()
          (set-contents!
           pc
           (get-contents reg)))))
     (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))


;;; assignment instruction selectors
  (define (assign-reg-name assign-instruction)
    (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

;; test instruction selectors
(define (test-condition test-instruction)
  (cdr test-instruction))

;;; branch instruction selectors
(define (branch-dest inst)
  (cadr inst))

;;; goto instruction selectors
(define (goto-dest inst)
  (cadr inst))

;;; advance program counter by one
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;;; make procedure for primitive expressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)            ;input: (const 1)
         (let ((c (const-exp-value exp))) ;ret: λ
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp)
                       )))
           (lambda () insts)))
        ((register-exp? exp)            ;input: (reg a)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r)))) ;ret: λ
        (else (error "Unkown expression type: ASSEMBLE" exp))))

;;; primitive expression selectors
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (const-exp-value exp)
  (cadr exp))
(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp)
  (cadr exp))

;;; make procedure for operational (non-primitive) expressions
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map
          (lambda (e)
            (if (label-exp? e)
                (error "Operations can be used with "
                       "registers and constants: ASSEMBLE" exp)
                (make-primitive-exp e machine labels)))
          (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

;;; operational expression selectors
;;;assign-inst: (assign val (op *) (reg b) (reg val))
;;;assign-reg-name: val
;;;assign-value-exp: ((op *) (reg b) (reg val))
(define (operation-exp? value-exp)
  (and (pair? value-exp)
       (tagged-list? (car value-exp) 'op)))
(define (operation-exp-op operation-exp) ;input: ((op *) (reg b) (reg val))
  (cadr (car operation-exp)))           ;ret: '*
(define (operation-exp-operands operation-exp) ;input: ((op *) (reg b) (reg val))
  (cdr operation-exp))                  ;ret: ((reg b) (reg val))


;;; lookup-prim: Sym x (listof (Sym λ)) -> λ
;;; look up primitive operation procedure in the operation table
(define (lookup-prim op operations)
  (cond ((null? operations)
         (error "Unknown operation: ASSEMBLE" op))
        ((eq? op (caar operations))
         (cadar operations))
        (else
         (lookup-prim op (cdr operations)))))
