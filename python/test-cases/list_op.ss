(define lst (list 1 2 3))

(=
 (car lst)
 1)


(=
 (cdr lst)
 (list 2 3))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons
       (car lst1)
       (append (cdr lst1) lst2))))

;; (display
;;  (append
;;   (list 1 2 3)
;;   (list 4 5 6))
;; )

