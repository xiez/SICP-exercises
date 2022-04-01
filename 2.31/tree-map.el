(defun tree-map (f tree)
  (cond ((null tree) nil)
        ((not (consp tree)) (funcall f tree))
        (t (cons (tree-map f (car tree))
                 (tree-map f (cdr tree))))))

(defun square-tree (tree)
  (funcall 'tree-map 'square tree))


(setq tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree tree)
