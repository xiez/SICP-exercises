;;; evaluator with lazy evaluation in section 4.2
#lang sicp
(#%require rackunit)

(define apply-in-underlying-scheme apply)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;; environment is made of a list of frames
;;; env selector & mutator --------------------
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (set-frame-binding! var val frame)
        true
        (add-binding-to-frame! var val frame))))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (if (set-frame-binding! var val frame)
          true
          (env-loop (enclosing-environment env))))
    (if (eq? env the-empty-environment)
        (error "Unbound vairable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

;;; env implementation with frame
;;; a frame is a pair of (car frame) points to the variables
;;; and (cdr frame) points to the values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

;set new-val of variable in the frame, return fales if variable is not found
(define (set-frame-binding! var new-val frame)
  (define (loop vars vals)
    (if (null? vars)
        false
        (if (eq? var (car vars))
              (begin
                (set-car! vals new-val)
                true)
              (loop (cdr vars) (cdr vals)))))
  (loop (frame-variables frame) (frame-values frame)))
;;; unset variable in the frame, return false if variable is not found
(define (unset-frame-binding! var frame)
  (define (loop vars vals)
    (if (null? (cdr vars))
        false
        (let ((prev-var (car vars))
              (prev-val (car vals)))
          (if (eq? (cadr vars) var)
              (begin
                (set-cdr! prev-var (cddr vars))
                (set-cdr! prev-val (cddr vals)))
              (loop (cdr vars) (cdr vals))))))
  (let ((vars (frame-variables frame))
        (vals (frame-values frame)))
    (cond ((null? vars) false)
          ((eq? (car vars) var)
           (begin
             (set-car! frame (cdr vars))
             (set-cdr! frame (cdr vals))))
          (else (loop vars vals)))))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; ;;; TEST frame
(define base-env
  (extend-environment (list '+ '- '* '=) (list + - * =) '()))
(lookup-variable-value '+ base-env)
(first-frame base-env)
(define-variable! '/ / base-env)
(define-variable! 'display display base-env)
(define-variable! 'true false base-env)
(set-variable-value! 'true true base-env)
(unset-frame-binding! 'true (first-frame base-env))

;;; lazy related abstraction --------------------

;; thunks
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

;; force a thunk
(define (actual-value exp env)
  (force-it (eval exp env)))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result
           ))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;;; eval ----------------------------------------
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((make-unbound? exp) (eval-make-unbound exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((while? exp) (eval (while->iter exp) env))
        ((application? exp)
         (apply_ (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (false? res)
  (or
   (eq? res 0)
   (eq? res false)
   (eq? res '())
   ))
(define (true? res)
  (not (false? res)))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (actual-value (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

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

;;; lambda
(define (make-procedure params body env)
  (list 'procedure params (scan-out-defines body) env))

(define (scan-out-defines proc-body)    ;exer 4.16
  ;; (lambda <vars>
  ;;   (define u <e1>)
  ;;   (define v <e2>)
  ;;   <e3>)
  ;; would be transformed into
  ;; (lambda <vars>
  ;;   (let ((u '*unassigned*)
  ;;         (v '*unassigned*))
  ;;     (set! u <e1>)
  ;;     (set! v <e2>)
  ;;     <e3>))
  (define (defines->let defines non-defines)
    (let ((define-vars (map definition-variable defines))
          (define-vals (map definition-value defines)))
      (list
       (list                            ;TODO: make-let
        'let
        (map (lambda (x) (list x ''*unassigned*)) define-vars)
        (sequence->exp
         (map (lambda (x y) (list 'set! x y)) define-vars define-vals))
        (sequence->exp non-defines)))))
  (let ((defines (filter definition? proc-body))
        (non-defines (filter
                      (lambda (x)
                        (not (definition? x))) proc-body)))
    (if (or (null? defines) (null? non-defines))
        proc-body
        (defines->let defines non-defines))))

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
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
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
(define (cond-arrow-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-arrow-clause? first) ;clause contains =>
            (let ((test (cond-predicate first))
                  (recipient (caddr first)))
              (make-if
               test
               (list recipient test)
               (expand-clauses rest)))

            (if (cond-else-clause? first) ;normal clause
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))
        )))

;;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (define (named-let? exp)
    (symbol? (cadr exp)))
  (define (let-name exp)                ;fib-iter
    (if (named-let? exp)
        (cadr exp)))
  (define (let-variables exp)           ;(a b count)
    (if (named-let? exp)
        (map car (caddr exp))
        (map car (cadr exp))))
  (define (let-initial-vals exp)        ;(1 0 n)
    (if (named-let? exp)
        (map cadr (caddr exp))
        (map cadr (cadr exp))))
  (define (let-body exp)                ;<BODY>
    (if (named-let? exp)
        (cdddr exp)
        (cddr exp)))
  (let ((variables (let-variables exp))
        (initial-vals (let-initial-vals exp))
        (body (let-body exp)))
    (if (named-let? exp)
        (let ((name (let-name exp)))         ;named let
          (sequence->exp
           (list
            (list 'define (cons name variables)
                  (car body))           ; !! <BODY> is ((if ...))
            (cons name initial-vals))))
        (cons                           ;normal let
         (make-lambda variables body)
         initial-vals))))


;;; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (let ((clauses (cadr exp))
        (body (cddr exp)))
    (define (loop clauses)
      (if (null? clauses)
          (make-begin body)
          (list 'let
                (cons (car clauses) '())
                (loop (cdr clauses)))))
    (loop clauses)))

;;; letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (define (letrec-params exp) (cadr exp))
  (define (letrec-body exp) (cddr exp))

  (let ((vars (map (lambda (x) (car x)) (letrec-params exp)))
        (vals (map (lambda (x) (cadr x)) (letrec-params exp))))
    (list
     'let
     (map (lambda (x) (list x ''*unassigned*)) vars)
      (sequence->exp
       (map (lambda (x y) (list 'set! x y)) vars vals))
      (sequence->exp
       (letrec-body exp)))))

;;; while
(define (while->iter exp)
  (define (while-body exp)
    (caddr exp))
  (define (while-pred exp)
    (cadr exp))
  (sequence->exp
   (list
    (list 'define '(iter)
          (while-body exp)
          (make-if (while-pred exp)
                   '(iter)
                   'false))
    (make-if (while-pred exp)
             '(iter)
             'false))))
(define (while? exp)
  (tagged-list? exp 'while))


;;; make-unbound
(define (make-unbound? exp)
  (tagged-list? exp 'make-unbound!))
(define (eval-make-unbound exp env)
  (let ((frame (first-frame env)))
    (unset-frame-binding! (cadr exp) frame)))

;;; apply ------------------------------
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (primitive-procedure? proc)
  (not (compound-procedure? proc)))                                 ;TODO
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme proc args))
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))
(define (procedure-body proc)
  (caddr proc))
(define (procedure-parameters proc)
  (cadr proc))
(define (procedure-environment proc)
  (cadddr proc))
(define (apply_ procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))
          ))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;;; TEST ----------------------------------------
(define env
  (extend-environment
   (list '+ '- '* '= 'not 'display 'true 'false 'assoc 'cadr '>)
   (list + - * = not display true false assoc cadr >)
   '()))

;;; self-evaluating
(check-equal? (eval 1 env) 1)
(check-equal? (eval "hello" env) "hello")

;;; quotes
(check-equal? (eval '(quote 1)  env) '1)
(check-equal? (eval '(quote 1)  env) 1)
(check-equal? (eval '(quote true)  env) 'true)
(check-equal? (eval '(quote hello)  env) 'hello)

;;; assignment
(eval
 '(define (try a b)
    (if (= a 0) 1 b)) env)

(check-equal?
 (eval
 '(try 0 (/ 1 0))
 env)
 1)


;;; if
(check-equal?
 (eval
  '(if (= 1 1) 'a 'b)
  env)
 'a)

(check-equal?
 (eval
  '(if (not (= 1 1)) 'a 'b)
  env)
 'b)

(check-equal?
 (eval
 '(if (= 1 1) 'a)
 env)
 'a)

(check-equal?
 (eval
 '(if (= 1 1)
      (if (= 1 2) 'a 'b)
      'c)
 env)
 'b)


;;; application
(check-equal?
 (eval '(+ 1 1) env)
 2)

;;; begin
(check-equal?
 (eval
  '(begin
     (+ 1 1)
     'ok
     )
  env)
 'ok)

(check-equal?
 (eval
 '(begin
    (+ 1 1)
    (if (= 1 1) 'a 'b)
    )
 env)
 'a)

;;; cond
(check-equal?
 (eval
 '(cond ((= 1 1) 'a)
        ((= 2 2) 'aa)
        (else 'b))
 env)
 'a)

(check-equal?
 (eval
 '(cond ((= 1 2) 'a)
        ((= 2 2) 'aa)
        (else 'b))
 env)
 'aa)

(check-equal?
 (eval
 '(cond ((= 1 2) 'a)
        ((= 2 3) 'aa)
        (else 'b))
 env)
 'b)

(check-equal?
 (eval
 '(cond ((assoc 'b '((a 1) (b 2))) => cadr)
        (else false))
 env)
 2)

(check-equal?
 (eval
 '(cond ((assoc 'c '((a 1) (b 2))) => cadr)
        (else false))
 env)
 false)

;;; lambda
(check-equal?
 (eval
 '((lambda () 1))
 env)
 1)

(check-equal?
  (eval
     '((lambda (a) a) 1)                ; `a` is transformed into thunk
     env) 
 1)

(check-equal?
 (force-it
  (eval
   '((lambda (a)
       (begin (display 'hello)
              a))
     'world)
   env)
  )
 'world
 )

;;; let, exer 4.6
(check-equal?
 (eval
  '(let ((a 1)
         (b 2))
     (+ a b))
  env)
 3)

(check-equal?
 (eval
 '(let ((a 1))
    (begin
      (set! a 42)
      a))
 env)
 42)


;;; let*, exer 4.7
(eval
 '(let* ((x 3)
         (y (+ x 2))
         (z (+ x y 5)))
    (+ x z)
    (* x z))
 env)

;;; named let, exer 4.8
(eval
 '(define (fib n)
   (fib-iter 1 0 n))
 env)
(eval
 '(define (fib-iter a b count)
  (if (= count 0)
      b                                 ;`b` is tranformed into thunk
      (fib-iter (+ a b) a (- count 1))))
 env)
(check-equal?
  (eval
   '(fib 5)
   env) 
 5)
;;; --------------------
(eval
 '(define (fib2 n)
   (let fib-iter ((a 1)
                  (b 0)
                  (count n))
     (if (= count 0)
         b                              ; `b` is tranformed into thunk
         (fib-iter (+ a b) a (- count 1)))))
 env)
(check-equal?
  (eval '(fib2 5) env)
 5)

;;; exer 4.9
;; (define (fib3 n)
;;   (let ((a 1)
;;         (prev-a 1)
;;         (b 0)
;;         (count n))
;;     (while (> count 0)
;;            (begin
;;              (set! prev-a a)
;;              (set! a (+ a b))
;;              (set! b prev-a)
;;              (set! count (- count 1))))
;;     b))
;; can be transformed to
;; (define (fib3 n)
;;   (let ((a 1)
;;         (prev-a 1)
;;         (b 0)
;;         (count n))
;;     (define (iter)
;;       (begin
;;         (set! prev-a a)
;;         (set! a (+ a b))
;;         (set! b prev-a)
;;         (set! count (- count 1)))
;;       (if (> count 0)
;;           (iter)
;;           'false))
;;     (if (> count 0)
;;         (iter)
;;         'false)
;;     b))

;; in particular,
;; (while (> count 0)
;;            (begin
;;              (set! prev-a a)
;;              (set! a (+ a b))
;;              (set! b prev-a)
;;              (set! count (- count 1))))
;; can be transformed to
;; (define (iter)
;;       (begin
;;         (set! prev-a a)
;;         (set! a (+ a b))
;;         (set! b prev-a)
;;         (set! count (- count 1)))
;;       (if (> count 0)
;;           (iter)
;;           'false))
;; (if (> count 0)
;;     (iter)
;;     'false)

(eval
 '(define (fib n)
    (let ((a 1)
          (prev-a 1)
          (b 0)
          (count n))
      (while (> count 0)
             (begin
               (set! prev-a a)
               (set! a (+ a b))
               (set! b prev-a)
               (set! count (- count 1))))
      b))
 env)

(check-equal?
 (eval
 '(fib 5)
 env)
 5)

;;; exer 4.13
(eval
 '(define a 1)
 env)
(check-equal?
 (eval 'a env)
 1)

(eval
 '(make-unbound! a)
 env)

;;; only removing the binding in the first frame makes more sense

;;; exer 4.16

(check-equal?
 (eval-sequence
 '(
   (define (a n)
     (+ n b c)
     (define b 3)
     (define c 4))
   (a 3)
   )
 env)
 10)

(check-equal?
 (eval-sequence
  '(
    (define (b n)
      (+ n n)
      )
    (b 3)
    )
  env)
 6)

(check-equal?
 (eval-sequence
  '(
    (define (f x)
      (define (even? n)
        (if (= n 0)
            true
            (odd? (- n 1))))
      (define (odd? n)
        (if (= n 0)
            false
            (even? (- n 1))))
      (even? x))
    (f 10)
    )
  env)
 true)

(check-equal?
 (eval-sequence
  '(
    (define (f2 x)
      (even? x)
      (define (even? n)
        (if (= n 0)
            true
            (odd? (- n 1))))
      (define (odd? n)
        (if (= n 0)
            false
            (even? (- n 1)))))
    (f2 10)
    )
  env)
 true)


;;; exer 4.17
;;; the *defines->let* created an extra lambda which result in the extra frame
;;; Maybe we can just re-order inner statements to move all the (define ..) to the top,
;;; so that when <e3> is evaluated all the variables are defined.

;;; exer 4.18
;;; this procedure will not work since when <e2> is evaluated, the `y` is still *unassigned*.
;;; the scanned out procedure showed in the text works since when <e2> is evaluated, `y` is set.

;;; exer 4.19
;; (eval
;;  '(let ((a 1))
;;    (define (f x)
;;      (define b (+ a x))
;;      (define a 5)
;;      (+ a b))
;;    (f 10))
;;  env
;;  )

;;; Ben's assert is intuitive, but break the simultaneous scope rule.
;;;
;;; Alyssa is "correct" after we scanned out the defines, and is the same result for Python
;;; <ipython-input-6-33496798f463> in f(x)
;;       2     a = 1
;;       3     def f(x):
;; ----> 4         b = a + x
;;       5         a = 5
;;       6         return a + b
;; UnboundLocalError: local variable 'a' referenced before assignment
;;;
;;; Eva's opinion is ideal. Considering mutual recursion and delay evalulation,
;;; it's not easy to implement, need some kind of directed graph to resolve the dependency of variables

;;; exer 4.20
;; '(letrec ((<var1> <exp1>)  (<varn> <expn>))
;;   <body>)
;; can be transformed to
;; (let ((<var1> '*unassigned*)
;;       (<varn> '*unassigned*))
;;   (set! <var1> (eval <exp1>))
;;   (set! <varn> (eval <expn>))
;;   <body>)

(check-equal?
 (eval
 '(letrec ((fact
            (lambda (n)
              (if (= n 1)
                  1
                  (* n (fact (- n 1)))))))
    (fact 5))
 env)
 120)

(check-equal?
 (eval-sequence
 '(
   (define (f x)
     (letrec ((even?
               (lambda (n)
                 (if (= n 0)
                     true
                     (odd? (- n 1)))))
              (odd?
               (lambda (n)
                 (if (= n 0)
                     false
                     (even? (- n 1))))))
       (odd? 11)))
   (f 11)
   )
 env)
 true)

(eval
 '((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
 env
 )

;;; exer 4.21 TODO
