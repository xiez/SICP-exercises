(define (cont-frac n d k)
  (define (frac-recur n d i)
    (cond ((> i k) (/ (n i) (d i)))
          (else (/ (n i)
                   (+ (d i) (frac-recur n d (+ i 1)))))))
  (frac-recur n d 1))

(define (cont-frac n d k)
  (define (iter n d k result)
    (cond ((= k 0) result)
          (else (iter n d (- k 1) (/ (n k)
                                     (+ (d k) result))))))

  (iter n d k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (n)
             (cond ((= (remainder n 3) 0) 1)
                   ((= (remainder n 3) 1) 1)
                   (else (* 2
                            (+ 1 (quotient n 3))))
                   ))
           10)


