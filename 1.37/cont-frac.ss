;; recursive
(define (cont-frac n d k)
  (define (frac-recur n d i)
    (cond ((> i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i)
                            (frac-recur n d (+ i 1)))))))
  (frac-recur n d 1))

;; iterative
(define (cont-frac n d k)
  (define (iter n d k result)
    (cond ((= k 0) result)
          (else (iter n d (- k 1) (/ (n k)
                                     (+ (d k) result))))))

  (iter n d k 0))

;; (iter n d 2 0)
;; (iter n d 1 (/ (n 2) (d 2)))
;; (iter n d 0 (/ (n 1) (+ (d 1) (/ (n 2) (d 2)))))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
