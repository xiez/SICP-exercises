#lang sicp
(#%require rackunit)

(define apply-in-underlying-scheme apply)

;;; env table  ----------------------------------------
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (car (car records))) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define env (make-table eq?))

;;; eval ----------------------------------------
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply_ (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ;; ((lambda? exp)
        ;;  (make-procedure (lambda-parameters exp)
        ;;                  (lambda-body exp)
        ;;                  env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))

        ;; ((and? exp) (eval-and exp env))
        ;; ((or? exp) (eval-or exp env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))

        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (lookup-variable-value var env)
  ((env 'lookup-proc) var))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (true? res)
  (eq? res #true))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (set-variable-value! var val env)
  'TODO)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (define-variable! var val)
  'TODO)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #true)
        ((string? exp) #true)
        (else #false)))

(define (variable? exp) (symbol? exp))

;;; (quote 'a)
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #false))
(define (text-of-quotation exp) (cadr exp))

;;; (set! a 1)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;; (define <var> <value>) or (define (<var> <param 1> ... <param n>) <body>)
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;;; (if predicate? <consequent> <alternative>)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;; (begin <exp 1> ... <exp n>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;;; (square 3) or (newline)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; (cond ((> x 0) x)
;;       ((= x 0) (display 'zero) 0)
;;       (else (- x)))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; apply ------------------------------
(define (primitive-procedure? proc)
  #true)                                 ;TODO
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme proc args))
(define (compound-procedure? proc)
  #true)
(define (procedure-body)
  'TODO)
(define (extend-environment a b env)
  'TODO)
(define (procedure-parameters proc)
  'TODO)
(define (procedure-environment proc)
  'TODO)
(define (apply_ procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;; TEST ----------------------------------------

((env 'insert-proc!) '= =)
((env 'insert-proc!) '+ +)
((env 'insert-proc!) 'not not)
((env 'insert-proc!) 'display display)
((env 'insert-proc!) 'true true)
((env 'insert-proc!) 'false false)

;;; self-evaluating
(check-equal? (eval 1 env) 1)
(check-equal? (eval "hello" env) "hello")

;;; quotes
(check-equal? (eval '(quote 1)  env) '1)
(check-equal? (eval '(quote 1)  env) 1)
(check-equal? (eval '(quote true)  env) 'true)
(check-equal? (eval '(quote hello)  env) 'hello)

;;; assignment

;;; if
(check-equal?
 (eval
  '(if (call = 1 1) 'a 'b)
  env)
 'a)

(check-equal?
 (eval
  '(if (call not (call = 1 1)) 'a 'b)
  env)
 'b)

(check-equal?
 (eval
 '(if (call = 1 1) 'a)
 env)
 'a)

(check-equal?
 (eval
 '(if (call = 1 1)
      (if (call = 1 2) 'a 'b)
      'c)
 env)
 'b)

;;; application
(check-equal?
 (eval '(call + 1 1) env)
 2)


;;; cond
(check-equal?
 (eval
  '(cond ((call = 1 1) 'a)
         ((call = 2 2) 'aa)
         (else 'b))
  env)
 'a)



;;; begin
(check-equal?
 (eval
  '(begin
     (call + 1 1)
     'ok
     )
  env)
 'ok)

(check-equal?
 (eval
 '(begin
    (call + 1 1)
    (if (call = 1 1) 'a 'b)
    )
 env)
 'a)

;;; exer 4.4 ========================================
;;; (add <exp 1> ... <exp n>)
(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))
(define (eval-and exp env)
  (define (eval-and_ predicates env)
    (cond ((null? predicates) true)
          ((not (true? (eval (first-exp predicates) env)))
           false)
          (else (eval-and_ (rest-exps predicates) env))))
  (eval-and_ (and-predicates exp) env))

;; (or ... )
(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (eval-or exp env)
  (define (eval-or_ predicates env)
    (cond ((null? predicates) false)
          ((true? (eval (first-exp predicates) env))
           true)
          (else (eval-or_ (rest-exps predicates) env))))
  (eval-or_ (or-predicates exp) env))

;;; test
;; (check-equal?
;;  (eval '(and (call = 1 1) (call = 1 2)) env)
;;  false)

;; (check-equal?
;;  (eval '(and (call = 1 1) (call = 1 2)) env)
;;  true)


;;; ========================================

(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))
(define (expand-and-predicates predicates)
  (if (null? predicates) 'true
      (let ((first-pred (car predicates))
            (rest-preds (cdr predicates)))
        (make-if
         first-pred
         (expand-and-predicates rest-preds)
         'false))))

(define (or->if exp)
  (expand-or-predicates (or-predicates exp)))
(define (expand-or-predicates predicates)
  (if (null? predicates) 'false
      (let ((first-pred (car predicates))
            (rest-preds (cdr predicates)))
        (make-if
         first-pred
         'true
         (expand-or-predicates rest-preds)))))

(check-equal?
 (eval '(and (call = 1 1) (call = 1 2)) env)
 false)

(check-equal?
 (eval '(or (call = 1 1) (call = 1 2)) env)
 true)
