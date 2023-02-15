#lang racket

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
           (define exp '((op +) (const 1) (reg a)))
           (define exp2 '((op +) (const 1) (label a)))
           (define machine '())
           (define labels '())
           (define operations (list (list '+ +)))

           (check-exn
            exn:fail?
            (lambda ()
              (make-operation-exp exp2 machine labels operations)))
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

