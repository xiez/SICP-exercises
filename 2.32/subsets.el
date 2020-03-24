(add-to-list 'load-path "..")
(require 'map "sicp")

(setq set (list 1 2 3))

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x)
                          (cons (car s) x))
                        rest)))))

;; the subsets of S is the combination of
;; 1) subsets of (cdr S) and
;; 2) append (car S) to every set in conditon 1

;; base condition: subsets of () is nil

;; e.g. subsets of (2 1) is the combination of
;; 1) subsets of (1) which is (nil (1)) and
;; 2) append 2 to every set in subsets of (1)
;; (map (lambda (x)
;;        (cons 2 x))
;;      (list (list) (list 1)))
