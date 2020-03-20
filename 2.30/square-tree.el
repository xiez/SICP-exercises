(add-to-list 'load-path "..")
(require 'map "sicp")

(setq tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defun scale-tree (tree factor)
  (cond ((null tree) nil)
        ((not (consp tree)) (* tree factor))
        (t (cons (scale-tree (car tree) factor)
                 (scale-tree (cdr tree) factor)))))

(defun scale-tree2 (tree factor)
  (map (lambda (sub-tree)
         (if (consp sub-tree)
             (scale-tree2 sub-tree factor)
           (* sub-tree factor)))
       tree))

(scale-tree2 tree 10)

;; ****************************************
(defun square-tree (tree)
  (cond ((null tree) nil)
        ((not (consp tree)) (square tree))
        (t (cons (square-tree (car tree))
                 (square-tree (cdr tree))))))


(defun square-tree2 (tree)
  (map (lambda (sub-tree)
         (if (consp sub-tree)
             (square-tree2 sub-tree)
           (square sub-tree)))
       tree))

(square-tree2 tree)
