(define (sum term a next b)
  (if (> a b)
      0
      (+
       (term a)
       (sum term (next a) next b))))

(define (cube x)
  (*
   (* x x)
   x))

(define (1+ a)
  (+ 1 a))

(define (sum-cubes a b)
  (sum cube a 1+ b))

(=
 36
 (sum-cubes 1 3))

;;; ------------------------------
(define (expt x n)
  (if (= n 0)
      1
      (* x (expt x (- n 1)))))

(define (sum-powers a b n)
  (define (nth-power x)
    (expt x n))
  (sum nth-power a 1+ b))

(=
 14
 (sum-powers 1 3 2))

