;; (0 0) 1
;; (1 0) (1 1) 1
;; (2 0) (2 2) 1, (2 2) 2

(define (pascal row col)
  (cond ((= col 0) 1)
        ((= col row) 1)
        ((> col row) 0)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(pascal 1 1)
(pascal 3 2)
(pascal 5 3)

