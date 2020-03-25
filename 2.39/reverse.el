(add-to-list 'load-path "..")
(require 'accumulate "sicp")

(fset 'fold-right 'accumulate)

(defun fold-left (op initial sequence)
  (defun iter(result rest)
    (if (null rest)
        result
      (iter (funcall op result (car rest))
            (cdr rest))
    ))
  (iter initial sequence))

;; ****************************************

(defun my-reverse (sequence)
  (fold-right (lambda (x y)
                (progn
                  (message "%s, %s" x y)
                  (append y (list x))))
              nil
              sequence))

(my-reverse (list 1 2 3))

(defun my-reverse2 (sequence)
  (fold-left (lambda (x y)
               (progn
                 (message "%s, %s" x y)
                 (cons y x)))
             nil
             sequence))


(my-reverse2 (list 1 2 3))
