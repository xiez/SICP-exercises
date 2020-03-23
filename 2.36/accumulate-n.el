(add-to-list 'load-path "..")
(require 'map "sicp")

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(accumulate '+ 0 (list 1 2 3))

(accumulate (lambda (x y)
              (progn
                (message "%s %s" x y)
                (* x y)
                ))
            1 (list 1 2 3))

;; ****************************************

(setq seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(defun accumulate-n (op init seqs)
  (defun transform-seq (seqs)
    ;; transform ((1 2 3) (4 5 6) (7 8 9) (10 11 12)) to
    ;; ((1 4 7 10) (2 5 8 11) (3 6 9 12))
    (defun iter (s res)
      (let ((first (map (lambda (x) (car x)) s))
            (rest (map (lambda (x) (cdr x)) s)))
        (message "%s" first)
        (if (null (car first))
            res
          (iter rest (append res (list first))))
        ))

    (iter seqs nil))

  (let ((res (transform-seq seqs)))
    (map (lambda (x) (accumulate op init x))
         res)))

(accumulate-n '+ 0 seqs)
