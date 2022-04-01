(defun make-triple-sum (n s)
  (filter (lambda (x) (= (apply '+ x) s))
          (accumulate 'append
                      nil
                      (flatmap
                       (lambda (i)
                         (map (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 (- j 1)) ))
                              (enumerate-interval 1 (- i 1))))
                       (enumerate-interval 1 n)))
          ))

(make-triple-sum 6 10)


(flatmap
 (lambda (i)
   (map (lambda (j)
          (map (lambda (k)
                 (list i j k))
               (enumerate-interval 1 (- j 1)) ))
        (enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 n))
