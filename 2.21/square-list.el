(add-to-list 'load-path "..")
(require 'square? "sicp")

(defun square-list (items)
  (if (null items)
      nil
    (cons (square (car items))
          (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(defun square-list2 (items)
  (mapcar 'square items))

(square-list2 (list 1 2 3 4))
