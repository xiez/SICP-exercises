(add-to-list 'load-path "..")
(require 'even? "sicp")
(require 'odd? "sicp")

(defun f (x y &rest trailing)
  (message "%s" trailing))

(f 1 2 3 4 5)

(defun g (&rest r)
  (if (= 0 (length r))
      (message "No args")
      (message "%s" r)))

(g)
(g 1 2 3)


;; ****************************************
(defun filter (lst test-func)
  (defun iter (lst test-func new-list)
    (if (= 0 (length lst))
        new-list
      (if (funcall test-func (car lst))
          (iter (cdr lst) test-func (append new-list (list (car lst))))
        (iter (cdr lst) test-func new-list))))
  (iter lst test-func (list)))

(filter (list 1 2 3 4 5)
        (lambda (x)
          (if (= 0 (mod x 2))
              t
            nil)))

(defun same-parity (x &rest y)
  (if (even? x)
      (append (list x) (filter y 'even?))
    (append (list x) (filter y 'odd?))
    ))

(same-parity 10 2 3 4 5 6 7)
