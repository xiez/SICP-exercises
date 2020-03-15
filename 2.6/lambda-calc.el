(defun zero ()
  (lambda (f) (lambda (x) x)))

(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))

(defun one ()
  (lambda (f) (lambda (x) (funcall f x))))

(defun two ()
  (lambda (f) (lambda (x) (funcall f (funcall f x)))))

;; (+ 1 b)
;; (lambda (f) (lambda (x) (funcall f (funcall (funcall b f) x))))

;; (+ 2 b) equals (+ 1 (+ 1 b))
;; (lambda (f) (lambda (x) (funcall f (funcall (funcall (+ 1 b) f) x))))
;; (lambda (f) (lambda (x) (funcall f (funcall
;;                                     (lambda (x) (funcall f (funcall (funcall b f) x))) x))))

;; (lambda (f) (lambda (x) (funcall f (funcall f (funcall (funcall b f) x)))))

(defun add (a b)
    (lambda (f) (lambda (x) (funcall (funcall a f) (funcall (funcall b f) x)))))
