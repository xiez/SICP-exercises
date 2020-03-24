(add-to-list 'load-path "..")
(require 'accumulate "sicp")

(fset 'fold-right 'accumulate)
(fold-right '+ 0 (list 1 2 3))

(defun fold-left (op initial sequence)
  (defun iter(result rest)
    (if (null rest)
        result
      (iter (funcall op result (car rest))
            (cdr rest))
    ))
  (iter initial sequence))

(fold-left '+ 0 (list 1 2 3))

;; ****************************************
(fold-right '/ 1 (list 1.0 2.0 3.0))

(fold-left '/ 1 (list 1.0 2.0 3.0))

(fold-right 'list nil (list 1 2 3))

(fold-left 'list nil (list 1 2 3))

;; ****************************************

(fold-right '* 1 (list 1 2 3))
(fold-left '* 1 (list 1 2 3))
