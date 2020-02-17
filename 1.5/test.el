(defun p () (p))

(defun test (x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;; result in Lisp error: (error "Lisp nesting exceeds ‘max-lisp-eval-depth’")
