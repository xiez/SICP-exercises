(setq x (list (list 1 2) (list 3 4)))

(defun my-reverse (l)
  (if (= 1 (length l))
      l
    (append (my-reverse (cdr l))
          (list (car l)))))

(my-reverse x)

;; ****************************************

(defun deep-reverse (lst)
  (cond
   ;; nil
   ((null lst) nil)
   ;; pair
   ((consp lst)
    (append (deep-reverse (cdr lst))
            (list (deep-reverse (car lst)))))
   ;; not pair
   (t lst)))

(deep-reverse x)

;; (f ((3)))

;; (append (f nil)
;;         (list (f (3))))

;; (append nil
;;         (list (f (3))))

;; ;; (f (3))
;; (append (f nil)
;;         (list (f 3)))

;; (append nil
;;         (list (f 3)))

;; (append nil
;;         (list 3))
