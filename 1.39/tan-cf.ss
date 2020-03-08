(define (tan-cf x k)
  ;; (define (n x k)
  ;;   (if (= k 1)
  ;;       x
  ;;       (* x x)))

  ;; (define (d k)
  ;;   (- (* 2 k) 1))
  
  (define (recur x i)
    (cond ((> i k) 0)
          (else (/
                 ;; (n x i)
                 ((lambda (x k) (if (= k 1)
                                    x
                                    (* x x)))
                  x
                  i)
                 (-
                  ;; (d i)
                  ((lambda (k)
                   (- (* 2 k) 1))
                  i)
                  (recur x (+ 1 i)))))))
  (recur x 1))

(tan-cf 10 10)
