(define tree
  (cons 1
        (cons 2
              (cons 3
                    (cons (cons 4 '())
                          (cons 5 '()))))))

(define tree
  (cons 1 '()
))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else 
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))
