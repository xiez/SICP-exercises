(define lst (list 1 2 3))

(=
 (car lst)
 1)


(=
 (cdr lst)
 (list 2 3))

(=
 null
 (cdr
  (cdr (cdr lst))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons
       (car lst1)
       (append (cdr lst1) lst2))))

(=
 (append
  (list 1 2 3)
  (list 4 5 6))
 (list 1 2 3 4 5 6))

(=
 (cons 1 (cons 2 null))
 (list 1 2)
 )


(define (length lst)
  (if (null? lst)
      0
      (+ 1
         (length (cdr lst)))))

(=
 (length (list 1 2 3))
 3
 )
