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
  (cadr inst))

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


(define (make-execution-procedure . args)
  (lambda () 'todo))
