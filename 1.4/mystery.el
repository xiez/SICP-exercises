;; ref: https://emacs.stackexchange.com/questions/14747/how-to-pass-function-as-argument-in-elisp
(defun mystery (a b)
  (funcall (if (> b 0) '+ '-) a b))
(mystery 3 -4)
(mystery 3 4)
