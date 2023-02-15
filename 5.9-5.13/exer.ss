;; 5.9
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

;; 5.10
;; TODO

;;; 5.11

;; 1

;; 2

