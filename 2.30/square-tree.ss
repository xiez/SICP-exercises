;;; map
(define (square-tree tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree sub-tree)
               (* sub-tree sub-tree)))
         tree))

;;; recursion
(define (square-tree2 tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (* tree tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))
