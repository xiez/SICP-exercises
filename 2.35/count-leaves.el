(defun count-leaves (x)
  (cond ((null x) 0)
        ((not (consp x)) 1)
        (t (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(setq tree (list (list 1) (list 2) (list (list 3 4))))
(count-leaves tree)

(defun count-leaves2 (tree)
  (accumulate
   (lambda (x y)
     (if (consp y)
         (progn
           (message "not leaf")
           (count-leaves2 y)
           )
       (progn
         (message "Reach leaf, count +1")
         (+ 1 y)
         )

       )
     )
   0
   tree))

(count-leaves2 tree)
