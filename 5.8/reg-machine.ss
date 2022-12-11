#lang sicp

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

;;; register
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

(define (get-contents reg)
  (reg 'get))
(define (set-contents! reg value)
  ((reg 'set) value))

;;; stack
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

;;; machine
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

;;; shortcut functions
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

;;; assembler

;; (define controller-text
;;   '(expt-loop
;;     (test (op =) (reg n) (const 0))
;;     (branch (label expt-base))
;;     (save n)
;;     (save continue)
;;     (assign n (op -) (reg n) (cons 1))  ;n = n -1
;;     (assign continue (label after-expt))
;;     (goto (label expt-loop))
;;     ;; expt-base
;;     expt-base
;;     (assign val (const 1))              ;val = 1
;;     (goto (reg continue))
;;     ;; after-expt
;;     after-expt
;;     (restore continue)
;;     (restore n)
;;     (assign val (op *) (reg b) (reg val)) ;val = b * (expt b (- n 1))
;;     (goto (reg continue))
;;     expt-done))

;; (define insts
;;   '(((test (op =) (reg n) (const 0)))
;;   ((branch (label expt-base)))
;;   ((save n))
;;   ((save continue))
;;   ((assign n (op -) (reg n) (cons 1)))
;;   ((assign continue (label after-expt)))
;;   ((goto (label expt-loop)))
;;   ((assign val (const 1)))
;;   ((goto (reg continue)))
;;   ((restore continue))
;;   ((restore n))
;;   ((assign val (op *) (reg b) (reg val)))
;;   ((goto (reg continue)))))

;; (define labels
;;   '((expt-loop
;;      ((test (op =) (reg n) (const 0)))
;;      ((branch (label expt-base)))
;;      ((save n))
;;      ((save continue))
;;      ((assign n (op -) (reg n) (cons 1)))
;;      ((assign continue (label after-expt)))
;;      ((goto (label expt-loop)))
;;      ((assign val (const 1)))
;;      ((goto (reg continue)))
;;      ((restore continue))
;;      ((restore n))
;;      ((assign val (op *) (reg b) (reg val)))
;;      ((goto (reg continue))))
;;     (expt-base
;;      ((assign val (const 1)))
;;      ((goto (reg continue)))
;;      ((restore continue))
;;      ((restore n))
;;      ((assign val (op *) (reg b) (reg val)))
;;      ((goto (reg continue))))
;;     (after-expt ((restore continue)) ((restore n)) ((assign val (op *) (reg b) (reg val))) ((goto (reg continue))))
;;     (expt-done)))


(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

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

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (make-instruction text)
  (cons text '()))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))

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


;;;
;;; make different (assign/goto/branch...) instruction procedure
(define (make-execution-procedure inst-text labels machine pc flag stack ops)
  ;; (set-contents! pc (cdr (get-contents pc)))
  (cond ((eq? (car inst-text) 'assign)
         (make-assign
          inst-text machine labels ops pc))
        (else
         (error "Unknown instruction type: ASSEMBLE" inst-text))))

;;; instruction ... -> λ
;;; make assignment instruction procedure
(define (make-assign inst machine labels ops pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp 
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()                      ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;;; Primitive assign
;;;assign-inst: (assign val (const 1))
;;;assign-reg-name: val
;;;assign-value-exp: ((const 1))
;;; or
;;;assign-inst: (assign val (reg a))
;;;assign-value-exp: ((reg a))
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)            ;input: (const 1)
         (let ((c (const-exp-value exp))) ;ret: λ
           (lambda () c)))
        ((register-exp? exp)            ;input: (reg a)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r)))) ;ret: λ
        (else (error "Unkown expression type: ASSEMBLE" exp))))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (const-exp-value exp)
  (cadr exp))
(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))

;;; Operational assign
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

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map
          (lambda (e) (make-primitive-exp e machine labels))
          (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))


;;; op-symbol (listof (op-symbol λ)) -> λ
;;; lookup primitive operation name in operation table
(define (lookup-prim op operations)
  (cond ((null? operations)
         (error "Unknown operation: ASSEMBLE" op))
        ((eq? op (caar operations))
         (cdar operations))
        (else
         (lookup-prim op (cdr operations)))))
