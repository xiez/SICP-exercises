(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(accumulate '+ 0 (list 1 2 3 4))

(defun map (p sequence)
  (accumulate
   (lambda (x y) (cons (funcall p x) y))
   nil
   sequence))

(defun my-append (seq1 seq2)
  (accumulate
   'cons
   seq2
   seq1))

(my-append (list 1 2) (list 3 4))


(defun length (sequence)
  (accumulate
   (lambda (x y) (+ 1 y))
   0
   sequence))
