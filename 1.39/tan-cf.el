(defun tan-cf (x k)
  ;; (defun (n x k)
  ;;   (if (= k 1)
  ;;       x
  ;;       (* x x)))

  ;; (defun (d k)
  ;;   (- (* 2 k) 1))
  
  (defun recur (x i)
    (cond ((> i k) 0)
          (t (/
              ;; (n x i)
              (float ((lambda (x k) (if (= k 1)
                                 x
                               (* x x)))
               x
               i))
              (-
               ;; (d i)
               ((lambda (i)
                  (- (* 2 i) 1))
                i)
               (recur x (+ 1 i)))))))
  (recur x 1))

(tan-cf 30 60)




