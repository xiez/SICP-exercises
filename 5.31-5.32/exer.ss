;;; 5.31

(f 'x 'y)
;;; save continue, env, unev before eval f
;;; restore unev and env after eval f
(save continue)
(save env)                              ;could be eleminated
(save unev)                             ;ditto
(restore unev)                          ;ditto
(restore env)                           ;ditto
;;; save proc before eval ('x 'y)
(save proc)                             ;could be eleminated, since proc is not changed
;;; save argl, env, unev before eval 'x
(save argl)                             ;could be eleminated, since argl is not changed during eval 'x
(save env)                              ;ditto
(save unev)                             ;ditto
;;; restore unev, env, argl after eval 'x
(restore unev)                          ;ditto
(restore env)                           ;ditto
(restore argl)                          ;ditto
;;; save argl before eval 'y
(save argl)                             ;could be eleminated, since argl is not changed during eval 'y
;;; restore argl, proc after eval 'y
(restore argl)                          ;ditto
(restore proc)                          ;ditto
;;; go apply-dispatch


((f) 'x 'y)
;;; ev-application ((f) 'x 'y), exp set to (f)
(save continue)
(save env)                              ;could be eleminated, no need to save env, since 'x and 'y are self evaluated
(save unev)                             ;<-- needed, since unev is change during eval (f)
;;; ev-application (f), exp set to f
(save continue)
(save env)                              ;could be eleminated
(save unev)                             ;could be eleminated
;;; eval f, lookup f in env, proc set to <proc-f>
;;; restore unev, env after eval f
(restore unev)                          ;could be eleminated
(restore env)                           ;could be eleminated
;;; compound-apply (f) ... unev set to <proc-params>, env is extended
;;; eval the body of f sequencely in new env ... exp set to last exp of proc, and val set to the value of last exp
;;; restore unev, env after apply (f), proc set to the value of last exp
(restore unev)                          ;<-- needed, restore ('x 'y)
(restore env)                           ;could be eleminated
(save proc)                             ;could be eleminated, proc is not changed during eval ('x 'y)
;;; ev-appl-operand-loop eval 'x
(save argl)                             ;could be eleminated
(save env)                              ;could be eleminated
(save unev)                             ;could be eleminated
;;; after eval 'x
(restore unev)                          ;ditto
(restore env)                           ;ditto
(restore argl)                          ;ditto
;;; ev-appl-operand-loop eval 'y
(save argl)                             ;ditto
(save env)                              ;ditto
(save unev)                             ;ditto
;;; after eval 'y
(restore argl)                          ;ditto
(restore proc)                          ;ditto
;;; go apply

;; (f (g 'x) y)
;;; env
;;; 1. saved before eval operator f, can be eleminated since env is not changed during eval f
;;; 2. saved before eval operand (g 'x), needed, since apply (g 'x) will change env
;;; 3. saved before eval operator g, can be eleminated since env is not changed during eval g
;;; 4. saved before eval operand 'x, can be eleminated since env is not changed during eval 'x
;;; 5. saved before eval y, can be eleminated since env is not changed during eval y
;;; argl
;;; 1. saved before eval (g 'x), needed, since eval 'x will change argl
;;; 2. saved before eval 'x, can be eleminated since argl is not trashed during eval 'x
;;; 3. saved before eval y, can be eleminated since argl is not trashed during eval y
;;; proc
;;; 1. saved before eval operand (g 'x), needed, since eval g will change proc
;;; 2. saved before eval operand 'x, can be eleminated since proc is not changed during eval 'x
;;; 3. saved before eval operand y, can be eleminated since proc is not changed during eval y
;;; TEST:
(define (f x y) (+ x y))
(define (g x) x)
(define y 1)
(f (g 2) y)

;; (f (g 'x) 'y)
;;; env
;;; 1. saved before eval operator f, can be eleminated since env is not changed during eval f
;;; 2. saved before eval operand (g 'x), can be eleminated, env is not needed during eval 'x and 'y
;;; 3. saved before eval operator g, can be eleminated since env is not changed during eval g
;;; 4. saved before eval operand 'x, can be eleminated since env is not changed during eval 'x
;;; 5. saved before eval y, can be eleminated since env is not changed during eval y
;;; argl
;;; 1. saved before eval (g 'x), needed, since eval 'x will change argl
;;; 2. saved before eval 'x, can be eleminated since argl is not trashed during eval 'x
;;; 3. saved before eval y, can be eleminated since argl is not trashed during eval y
;;; proc
;;; 1. saved before eval operand (g 'x), needed, since eval g will change proc
;;; 2. saved before eval operand 'x, can be eleminated since proc is not changed during eval 'x
;;; 3. saved before eval operand y, can be eleminated since proc is not changed during eval y
;;; TEST:
(define (f x y) (+ x y))
(define (g x) x)
(f (g 2) 1)


;;; 5.32

;;; a

(
 ;; ...

 ;; evaluate an application
 ev-application
 (save continue)                        ;set up for evaluating application operator

 (assign exp (op combination-operator) (reg exp))
 (assign unev (op combination-operands) (reg exp))
 (test (op is-symbol?) (reg exp))       ;is operator is a symbol, go eval without save env
 (branch (label ev-application-symbol-operator))
 (save env)                             ;otherwise, save current env since evaluate combination operator may change env
 (save unev)                            ;save un-evaluated operands
 (assign continue (label ev-appl-did-operator))
 (goto (label eval-dispatch))

 ev-application-symbol-operator
 (assign continue (label ev-appl-did-symbol-operator))
 (goto (label eval-dispatch))

 ev-appl-did-symbol-operator
 (assign argl (op empty-arglist))       ;init argl to empty list
 (assign proc (reg val))                ;get the procedure stored in reg val
 (test (op no-operands?) (reg unev))    ;if application without arguments
 (branch (label apply-dispatch))        ;go apply the application
 (save proc)                            ;otherwise, go evaluate the args
 
 ;; ...
)

;;; b

;; Alyssa is wrong.
;;; The interpreter always interprets every operator and operand each time the code is run, regardless of how many optimizations are incorporated. In contrast, a compiler "interprets" the code only once and generates object code that can be run multiple times.



