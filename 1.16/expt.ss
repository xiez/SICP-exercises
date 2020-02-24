(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt-iter b n)
  (define (expt-iter-aux b counter product)
    (if (= counter 0) product
        (expt-iter-aux b (- counter 1) (* b product))))
  (expt-iter-aux b n 1))

(define (fast-expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; iterative fast exponentiation
;; (f 2 10 1)
;; (f 4 5 1)
;; (f 4 4 4)
;; (f 16 2 4)
;; (f 32 1 4)
;; (* 32 4)

(define (fast-expt-iter b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (fast-expt-iter-aux b counter a)
    (cond ((= counter 1) (* b a))
          ((even? counter) (fast-expt-iter-aux (square b) (/ counter 2) a))
          (else (fast-expt-iter-aux b (- counter 1) (* b a)))))
  (fast-expt-iter-aux b n 1))

(fast-expt-iter 2 20)

