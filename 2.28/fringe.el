(setq x (list (list 1 2) (list 3 4)))

(defun fringe (lst)
  (cond
   ;; nil
   ((null lst) nil)
   ;; pair
   ((consp lst)
    (append
     (fringe (car lst))
     (fringe (cdr lst))))
   ;; not pair
   (t (list lst))
   ))

(fringe x)
(fringe (list x x))
