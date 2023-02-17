#lang racket
;;; raco test -t test_machine.rkt
(require rackunit)

(require "machine.ss")

(test-case "tagged-list?"
           (define lst '(a b c))
           (check-equal?
            (tagged-list?
             lst 'a)
            #t)
           (check-equal?
            (tagged-list?
             lst 'b)
            #f)
           (check-equal?
            (tagged-list?
             lst 'd)
            #f)
           )

(test-case "lookup-prim"
           (define ops (list
                        (list '+ +)
                        (list '- -)))

           (check-equal?
            (lookup-prim '+ ops)
            +)

           (check-exn
            exn:fail?
            (lambda ()
              (lookup-prim '/ ops)
              ))
           )

(test-case "operation-exp-selectors"
           (define exp '((op +) (const 1) (reg a)))
           (check-equal?
            (operation-exp-operands exp)
            '((const 1) (reg a))
            )
           )

(test-case "make-operation-exp"
           (define machine (make-machine
                            '(a)
                            '()
                            '()
                            ))
           (define labels '())
           (define operations (list (list '+ +)))

           (let ((proc
                  (make-operation-exp
                   '((op +) (const 1) (const 2))
                   machine labels operations)
                  ))
             (check-equal?
              (proc)
              3
              ))

           (let ((proc
                  (make-operation-exp
                   '((op +) (const 1) (reg a))
                   machine labels operations)
                  ))
             (set-contents! (get-register machine 'a) 42)
             (check-equal?
              (proc)
              43
              ))

           (check-exn
            exn:fail?
            (lambda ()
              (make-operation-exp
               '((op +) (const 1) (label a))
               machine labels operations)))

)



(test-case "stack"
           (define s (make-stack))

           ((s 'push) 1)
           (check-equal?
            (s 'top)
            1
            )

           ((s 'push) '(1 reg-a))
           (check-equal?
            (s 'top)
            '(1 reg-a)
            )

           (check-equal?
            (s 'pop)
            '(1 reg-a)
            )

           (check-equal?
            (s 'top)
            1
            )
           )

(test-case "assign-exp"
           (define inst-text '(assign a (const 1)))

           (check-equal?
            (assign-reg-name inst-text)
            'a
            )

           (check-equal?
            (assign-value-exp inst-text)
            '((const 1))
            )
           )

(test-case "op-exp"
           (check-equal?
            (operation-exp? '((op =) (reg a) (const 1)))
            #t
            )

           (check-equal?
            (operation-exp? '((const 1)))
            #f
            )

           )

(test-case "make-primitive-exp"

           )

(test-case "labels"
           (define insts-0
             '(
               (assign a (const 1))
               (test (op =) (const 1) (const 2))
               )
             )

           (define entry-0
             (make-label-entry
              'e0 insts-0))

           (define insts-1
             '(
               (assign b (const 1))
               (test (op >) (const 1) (const 2))
               ))

           (define entry-1
             (make-label-entry
              'e1 insts-1
              ))

           (define labels
             (list entry-0 entry-1))


           (check-equal?
            (lookup-label labels 'e0)
            insts-0
            )


           (check-equal?
            (lookup-label labels 'e1)
            insts-1
            )


           (check-exn
            exn:fail?
            (lambda ()
              (lookup-label labels 'e2)
              ))

           )

(test-case "extract-labels"
           (define text
             '(controller
               (assign a (const 1))
               done
               (goto (reg b))))

           (let ((receive-insts (lambda (insts labels) insts)))
             (check-equal?
              (extract-labels
               text
               receive-insts)
              '(
                ((assign a (const 1)) . *unset-proc*)
                ((goto (reg b)) . *unset-proc*)
                )))

           (let ((receive-labels (lambda (insts labels) labels)))
             (check-equal?
             (extract-labels
              text
              receive-labels)
             '(
               (controller
                ((assign a (const 1)) . *unset-proc*)
                ((goto (reg b)) . *unset-proc*))
               (done ((goto (reg b)) . *unset-proc*)))
             ))

           )

(test-case "make-execution-procedure"
           (define machine (make-machine
                            '(a)
                            (list (list '+ +) (list '> >))
                            '()
                            ))

           ;; assign primitive
           (let ((proc (make-execution-procedure
                        '(assign a (const 42))
                        '()
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )) )
             (set-contents! (get-register machine 'pc) '(done))
             (proc)

             (check-equal?
              (get-contents (get-register machine 'a))
              42
              )
             )

           ;; assign operation
           (let ((proc (make-execution-procedure
                        '(assign a (op +) (const 1) (const 2))
                        '()
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )) )
             (set-contents! (get-register machine 'pc) '(done))
             (proc)

             (check-equal?
              (get-contents (get-register machine 'a))
              3
              )
             )

           ;; test
           (let ((proc (make-execution-procedure
                        '(test (op >) (const 1) (const 2))
                        '()
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )))
             (set-contents! (get-register machine 'pc) '(done))

             (check-equal?
              (get-contents (get-register machine 'flag))
              '*unassigned*
              )

             (proc)

             (check-equal?
              (get-contents (get-register machine 'flag))
              #f
              )
             )

           ;; branch
           (let ((proc (make-execution-procedure
                        '(branch (label done))
                        '((done (assign a (const 1))))
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )))
             (set-contents! (get-register machine 'flag) #t)

             (check-equal?
              (get-contents (get-register machine 'pc))
              '()
              )

             (proc)                     ;set pc to label destination

             (check-equal?
              (get-contents (get-register machine 'pc))
              '((assign a (const 1)))
              )

             (set-contents! (get-register machine 'flag) #f)

             (proc)                     ;advance pc by 1

             (check-equal?
              (get-contents (get-register machine 'pc))
              '()
              )
             
             )

           ;; goto a label
           (let ((proc (make-execution-procedure
                        '(goto (label done))
                        '((done (assign a (const 1))))
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )))

             (proc)                     ;set pc to label destination

             (check-equal?
              (get-contents (get-register machine 'pc))
              '((assign a (const 1)))
              )
             )

           ;; goto a register
           (let ((proc (make-execution-procedure
                        '(goto (reg a))
                        '()
                        machine
                        (get-register machine 'pc)
                        (get-register machine 'flag)
                        (machine 'stack)
                        (machine 'operations)
                        )))

             (set-contents! (get-register machine 'a)
                            '((assign a (const 2))))

             (proc)                     ;set pc to register content
             (check-equal?
              (get-contents (get-register machine 'pc))
              '((assign a (const 2)))))



           )

