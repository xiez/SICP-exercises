;; recursive
(defun cont-frac (n d k)
  (defun frac-recur (n d i)
    (cond ((> i k) (/ (funcall n i) (funcall d i)))
          (t (/ (funcall n i)
                (+ (funcall d i) (frac-recur n d (+ i 1)))))))
  (frac-recur n d 1))

;; iterative
(defun cont-frac (n d k)
  (defun iter (n d k result)
    (cond ((= k 0) result)
          (t (iter n d (- k 1) (/ (funcall n k)
                                  (+ (funcall d k) result))))))

  (iter n d k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)
