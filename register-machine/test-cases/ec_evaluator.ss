(controller
 (assign env (op initial-env))
 (assign continue (label done))

 ;; REPL
 (goto (label read-eval-print-loop))

 eval-dispatch
 (test (op self-evaluating?) (reg exp))
 (branch (label ev-self-eval))
 (test (op variable?) (reg exp))
 (branch (label ev-variable))
 (test (op lambda?) (reg exp))
 (branch (label ev-lambda))
 (test (op definition?) (reg exp))
 (branch (label ev-definition))
 (test (op if?) (reg exp))
 (branch (label ev-if))
 (test (op let?) (reg exp))
 (branch (label ev-let))
 (test (op cond?) (reg exp))
 (branch (label ev-cond))
 (test (op application?) (reg exp))
 (branch (label ev-application))
 (goto (label unknown-expression-type))

 ev-self-eval
 (assign val (reg exp))
 (goto (reg continue))

 ev-variable
 (assign val (op lookup-variable-value) (reg exp) (reg env))
 (test (op unbound-variable?) (reg val))
 (branch (label ev-variable-unbound))
 (assign val (op bound-variable-value) (reg val))
 (goto (reg continue))

 ev-variable-unbound
 (assign val (const "Unbound-variable"))
 (goto (label signal-error))

 ev-lambda
 (assign unev (op lambda-parameters) (reg exp))
 (assign exp (op lambda-body) (reg exp))
 (assign val
         (op make-procedure)
         (reg unev) (reg exp) (reg env))
 (goto (reg continue))

 ;; evaluate an application
 ev-application
 (save continue)                        ;set up for evaluating application operator
 (save env)                             ;save current env since evaluate combination operator may change env
 (assign unev (op combination-operands) (reg exp))
 (save unev)                            ;save un-evaluated operands
 (assign exp (op combination-operator) (reg exp))
 (assign continue (label ev-appl-did-operator))
 (goto (label eval-dispatch))

 ;; evaluate application operands after operator
 ev-appl-did-operator
 (restore unev)                         ;restore the un-evaluated operands
 (restore env)                          ;restore the application env
 (assign argl (op empty-arglist))       ;init argl to empty list
 (assign proc (reg val))                ;get the procedure stored in reg val
 (test (op no-operands?) (reg unev))    ;if application without arguments
 (branch (label apply-dispatch))        ;go apply the application
 (save proc)                            ;otherwise, go evaluate the args

 ;; evaluating the args
 ev-appl-operand-loop
 (save argl)
 (assign exp (op first-operand) (reg unev)) ;get the first operand
 (test (op last-operand?) (reg unev))  ;if there is only one argument
 (branch (label ev-appl-last-arg))      ;go evaluate last argument (BASE CASE)
 (save env)                             ;otherwise, save env since eval next operand may change env
 (save unev)
 (assign continue (label ev-appl-accumulate-arg)) ;set up return address
 (goto (label eval-dispatch))           ;go eval first operand

 ;; evaluate last argument
 ev-appl-last-arg
 (assign continue (label ev-appl-accum-last-arg))
 (goto (label eval-dispatch))

 ;; accumulate last value to arglist (BASE CASE)
 ev-appl-accum-last-arg
 (restore argl)
 (assign argl (op adjoin-arg) (reg val) (reg argl))
 (restore proc)
 (goto (label apply-dispatch))          ;go apply the application

 ;; accumulating the argument values to arglist (RECURSIVE)
 ev-appl-accumulate-arg
 (restore unev)
 (restore env)
 (restore argl)
 (assign argl (op adjoin-arg) (reg val) (reg argl)) ;adjoin the eval result to argl
 (assign unev (op rest-operands) (reg unev)) ;remove first operand from unev
 (goto (label ev-appl-operand-loop))    ;go next loop
 ;; end ev-application

 ;; evaluate expressions in unev sequentialy
 ev-sequence
 (assign exp (op first-exp) (reg unev))
 (test (op last-exp?) (reg unev))
 (branch (label ev-sequence-last-exp))
 (save env)                             ;set up for eval first exp
 (save unev)
 (assign continue (label ev-sequence-continue))
 (goto (label eval-dispatch))

 ev-sequence-continue
 (restore unev)
 (restore env)
 (assign unev (op rest-exps) (reg unev)) ;remove first unevaluated exp
 (goto (label ev-sequence))             ;go eval next exp

 ev-sequence-last-exp                   ;<-- TAIL RECURSION
 (restore continue)
 (goto (label eval-dispatch))           ;goto eval without saving env and unev
 ;; end ev-sequence

 ;; ;; evaluate expressions WITHOUT tail recursion
 ;; ev-sequence
 ;; (test (op no-more-exp?) (reg unev))
 ;; (branch (label ev-sequence-finished))
 ;; (save env)                             ;set up for eval first exp
 ;; (save unev)
 ;; (assign exp (op first-exp) (reg unev))
 ;; (assign continue (label ev-sequence-continue))
 ;; (goto (label eval-dispatch))

 ;; ev-sequence-continue
 ;; (restore unev)
 ;; (restore env)
 ;; (assign unev (op rest-exps) (reg unev)) ;remove first unevaluated exp
 ;; (goto (label ev-sequence))             ;go eval next exp

 ;; ev-sequence-finished
 ;; (restore continue)
 ;; (goto (reg continue))           ;goto eval without saving env and unev
 ;; ;; end ev-sequence


 ;; apply application
 apply-dispatch
 (test (op primitive-procedure?) (reg proc))
 (branch (label primitive-apply))
 (test (op compound-procedure?) (reg proc))
 (branch (label compound-apply))
 (goto (label unknown-procedure-type))

 primitive-apply
 (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 (restore continue)
 (goto (reg continue))

 compound-apply
 (assign unev (op procedure-parameters) (reg proc)) ;get procedure parameters
 (assign env (op procedure-environment) (reg proc)) ;get procedure env
 (assign env (op extend-environment)    ;extends env by params and arg list
         (reg unev)
         (reg argl)
         (reg env))
 (assign unev (op procedure-body) (reg proc)) ;go eval procedure body by sequence
 (goto (label ev-sequence))             ; in new env (MUTUAL RECURSION)

 unknown-procedure-type
 (restore continue)
 (assign val (const "Unknown-procedure-type"))
 (goto (label signal-error))
 ;; end apply-dispatch

 ;; eval definition
 ev-definition
 (assign unev (op definition-variable) (reg exp))
 (save unev)
 (assign exp (op definition-value) (reg exp))
 (save env)
 (save continue)
 (assign continue (label ev-definition-1))
 (goto (label eval-dispatch))           ;evaluate definition value

 ev-definition-1
 (restore continue)
 (restore env)
 (restore unev)
 (perform (op set-variable-value!)
          (reg unev)
          (reg val)
          (reg env))
 (assign val (const ok))
 (goto (reg continue))
 ;; end eval definition

 ;; eval if
 ev-if
 (save exp)
 (save env)
 (save continue)
 (assign continue (label ev-if-decide))
 (assign exp (op if-predicate) (reg exp))
 (goto (label eval-dispatch))           ;evaluate if predicate

 ev-if-decide
 (restore continue)
 (restore env)
 (restore exp)
 (test (op true?) (reg val))
 (branch (label ev-if-consequent))

 ev-if-alternative
 (assign exp (op if-alternative) (reg exp))
 (goto (label eval-dispatch))

 ev-if-consequent
 (assign exp (op if-consequent) (reg exp))
 (goto (label eval-dispatch))
 ;; end eval if

 ;; eval let
 ev-let
 (assign exp (op let->combination) (reg exp))
 (goto (label eval-dispatch))
 ;; end eval let

 ;; ;; eval cond
 ;; ev-cond
 ;; (assign exp (op cond->if) (reg exp))
 ;; (goto (label eval-dispatch))
 ;; ;; end eval cond

 ;; eval cond (without reducing to if)
 ev-cond
 (save continue)
 (assign unev (op cond-clauses) (reg exp))

 ev-cond-test-pred-loop
 (assign exp (op first-clause) (reg unev)) ;get the first clause
 (test (op cond-else-clause?) (reg exp)) ;if it's else clause, go eval else clause
 (branch (label ev-cond-else-clause))   ;otherwise, go eval predicate of the clause
 (save continue)
 (assign continue (label ev-cond-pred-clause-1))
 (save unev)
 (save exp)
 (assign exp (op cond-predicate) (reg exp))
 (goto (label eval-dispatch))

 ev-cond-pred-clause-1
 (restore exp)
 (restore unev)
 (restore continue)
 (test (op true?) (reg val))            ;if the eval result is true
 (branch (label ev-cond-pred-clause-2))         ;eval clause actions
 (assign unev (op rest-clauses) (reg unev)) ;otherwise, remove first clause
 (goto (label ev-cond-test-pred-loop))  ;go next loop

 ev-cond-pred-clause-2
 (assign unev (op cond-actions) (reg exp)) ;get clause actions
 (goto (label ev-sequence))             ;go eval actions sequencially

 ev-cond-else-clause
 (assign unev (op cond-actions) (reg exp)) ;get clause actions
                                        ; (TODO: 'else' should be the last clause)
 (goto (label ev-sequence))             ;go eval actions sequencially
 ;; end eval cond

 ;; driver loop
 read-eval-print-loop
 (perform (op initialize-stack))
 (assign exp (op parse-read) (const "EC-Eval>>>"))
 (assign continue (label print-result))
 (goto (label eval-dispatch))

 print-result
 (perform (op print-stack-statistics))
 ;; (perform (op reset-stack-statistics))
 (perform (op print) (reg val))
 (goto (label read-eval-print-loop))
 ;; end driver loop

 unknown-expression-type
 (assign val (const "Unknown-expression-type"))
 (goto (label signal-error))

 signal-error
 (perform (op print) (const "ERROR:") (reg val) (const "EXP:") (reg exp))
 (goto (label read-eval-print-loop))

 done
 (perform (op print) (reg val))
 )
