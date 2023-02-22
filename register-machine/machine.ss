#lang sicp

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
;;; stack shortcut functions
(define (push stack content)
  ((stack 'push) content))
(define (pop stack)
  (stack 'pop))

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

;;; Create an initial instructions and a label table.
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
       ;; (display inst)
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
  (cons text '*unset-proc*)
  ;; (cons text '())
  )

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
        ((eq? (car inst-text) 'save)
         (make-save
          inst-text machine stack pc))
        ((eq? (car inst-text) 'restore)
         (make-restore
          inst-text machine stack pc))
        ((eq? (car inst-text) 'perform)
         (make-perform
          inst-text machine labels ops pc))
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

;; make procedure for save instruction
(define (make-save inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

;; make procedure for restore instruction
(define (make-restore inst machine stack pc)
  (let ((reg (get-register
              machine
              (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

;; make procedure for perform instruction
(define (make-perform inst machine labels ops pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action
                machine
                labels
                ops)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBL" inst))))


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

;;; save instruction selectors
(define (stack-inst-reg-name inst)
  (cadr inst))


;;; perform instruction selectors
(define (perform-action inst)
  (cdr inst))

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

;;; exports
;; (#%require racket/base)
;; (provide (all-defined-out))

;;; -------------------- test cases --------------------

;;; iterative expt machine
(let ((controller-text
       '(controller
         (assign b (const 2))                ;base = 2
         (assign counter (const 10))         ;counter = n
         (assign product (const 1))          ;product = 1
         expt-loop
         (test (op =) (reg counter) (const 0))
         (branch (label expt-done))
         (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
         (assign product (op *) (reg b) (reg product)) ;product = product * b
         (goto (label expt-loop))
         expt-done
         )
       )
      (registers '(b counter product))
      (ops (list
            (list '= =)
            (list '- -)
            (list '* *))))

  (define machine
    (make-machine registers ops controller-text))
  (machine 'start)
  (let ((res (get-register-contents
              machine
              'product)))

    (if (= 1024 res)
        (display "OK: iterative expt-machine result is 1024 \n")
        (error "Error: expt-machine result is " res)))
  )

;;; recursive expt machine
(let ((controller-text
       '(controller
         (assign continue (label expt-done))
         (assign n (const 10))
         (assign b (const 2))
         expt-loop
         (test (op =) (reg n) (const 0))
         (branch (label expt-base))
         (save n)
         (save continue)
         (assign n (op -) (reg n) (const 1))     ;n = n -1
         (assign continue (label after-expt))
         (goto (label expt-loop))
         expt-base
         (assign val (const 1))                 ;val = 1
         (goto (reg continue))
         after-expt
         (restore continue)
         (restore n)
         (assign val (op *) (reg b) (reg val))   ;val = b * (expt b (- n 1))
         (goto (reg continue))
         expt-done
         )
       )
      (registers '(continue b n val))
      (ops (list
            (list '= =)
            (list '- -)
            (list '* *))))

  (define machine
    (make-machine registers ops controller-text))
  (machine 'start)

  (let ((res (get-register-contents
              machine
              'val)))
    (if (= 1024 res)
        (display "OK: recursive expt-machine result is 1024 \n")
        (error "Error: expt-machine result is " res)))  
  )

;;; sqrt machine
(let* ((controller-text
        '(sqrt-loop
          (assign x (op read))
          (assign guess (const 1.0))
          test-guess
          (test (op good-enough?) (reg guess) (reg x))
          (branch (label done))
          (assign guess (op improve) (reg guess) (reg x))
          (goto (label test-guess))
          done
          (perform (op print) (reg guess)))
        )
       (registers '(x guess))
       (good-enough? (lambda (guess val)
                       (display (string-append
                                 "guess: " (number->string guess)
                                 " val:" (number->string val) "\n"))
                       (<
                        (abs (- (* guess guess) val) )
                        0.001)))
       (improve (lambda (guess val)
                  (/
                   (+ guess
                      (/ val guess))
                   2)))
       (ops (list
             (list 'good-enough? good-enough?)
             (list 'improve improve)
             (list 'print display)
             (list 'read read))))

  (define machine
    (make-machine registers ops controller-text))
  (machine 'start))

;;; sqrt machine 2
(let ((controller-text
        '(controller
          (assign x (op read))
          (assign guess (const 1.0))

          sqrt-loop
          (assign a (op square) (reg guess))     ;square is considered as a primitive op
          (assign a (op -) (reg a) (reg x))
          (assign a (op abs) (reg a))
          (test (op <) (reg a) (const 0.001))    ;if good-enough?, goto done with result in reg guess
          (branch (label done))
          (assign a (op /) (reg x) (reg guess))  ;otherwise, improve guess
          (assign guess (op average) (reg guess) (reg a))
          (goto (label sqrt-loop))

          done
          (perform (op print) (reg guess)))
        )
       (registers '(a x guess))
       (ops (list
             (list '- -)
             (list '/ /)
             (list '< <)
             (list 'abs abs)
             (list 'average (lambda (a b) (/ (+ a b) 2)))
             (list 'square (lambda (x) (* x x)))
             (list 'print display)
             (list 'read read))))

  (define machine
    (make-machine registers ops controller-text))
  (machine 'start))

;;; fib machine
;; (define (fib n)
;;   (if (< n 2)
;;       n
;;       (+ (fib (- n 1)) (fib (- n 2)) )))
;;;

(let ((controller-text
       '(controller
         (assign n (const 20))
         (assign continue (label fib-done))
         fib-loop
         (test (op <) (reg n) (const 2))
         (branch (label immediate-answer))
         ;; set up to compute Fib(n − 1)
         (save continue)
         (assign continue (label afterfib-n-1))
         (save n)                       ; save old value of n
         (assign n 
                 (op -)
                 (reg n)
                 (const 1))             ; clobber n to n-1
         (goto 
          (label fib-loop))     ; perform recursive call
         afterfib-n-1           ; upon return, val contains Fib(n − 1)
         (restore n)
         ;; (restore continue)
         ;; set up to compute Fib(n − 2)
         (assign n (op -) (reg n) (const 2))
         ;; (save continue)
         (assign continue (label afterfib-n-2))
         (save val)                     ; save Fib(n − 1)
         (goto (label fib-loop))
         afterfib-n-2           ; upon return, val contains Fib(n − 2)
         (assign n 
                 (reg val))             ; n now contains Fib(n − 2)
         (restore val)                  ; val now contains Fib(n − 1)
         (restore continue)
         (assign val                    ; Fib(n − 1) + Fib(n − 2)
                 (op +) 
                 (reg val)
                 (reg n))
         (goto                          ; return to caller,
          (reg continue))               ; answer is in val
         immediate-answer
         (assign val 
                 (reg n))               ; base case: Fib(n) = n
         (goto (reg continue))
         fib-done))
      (registers '(n val continue))
      (ops (list
            (list '< <)
            (list '- -)
            (list '+ +))))
  (define machine
    (make-machine registers ops controller-text))
  (machine 'start)

  (let ((res (get-register-contents
              machine
              'val)))
    (if (= 6765 res)
        (display "OK: fib-machine result is 6765 \n")
        (error "Error: fib-machine result is " res)))
  )
