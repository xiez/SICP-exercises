(define (double f)
  (lambda (x)
    (f (f x))))



;; ((double (double double)) add1)

;; ((double (lambda (x) (double (double x)))) add1)

;; ((lambda (x)
;;   ((lambda (x) (double (double x)))
;;    ((lambda (x) (double (double x))) x)))
;;  add1)

;; (lambda (x)
;;   ((lambda (x) (double (double x)))
;;    (add4 x)))

;; (double add8)

;; (add16)

