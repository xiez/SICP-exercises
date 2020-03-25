(defun remainder (a b)
  (if (>= a 0)
      (mod a (abs b))
    (- (mod (abs a) (abs b)))))

(defun gcd (a b)
  (if (= b 0)
      a
    (gcd b (remainder a b))))

(defun average (a b)
  (/ (float (+ a b)) 2))

(defun even? (x)
  (if (= 0 (mod x 2))
      t
    nil))

(defun odd? (x)
  (if (= 1 (mod x 2))
      t
    nil))

(defun square (n)
  (* n n))

(defun map (proc items)
  (if (null items)
      nil
    (cons (funcall proc (car items))
          (map proc (cdr items)))))

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
             (accumulate op initial (cdr sequence)))))

(defun filter (test-func lst)
  (defun iter (lst test-func new-list)
    (if (= 0 (length lst))
        new-list
      (if (funcall test-func (car lst))
          (iter (cdr lst) test-func (append new-list (list (car lst))))
        (iter (cdr lst) test-func new-list))))
  (iter lst test-func (list)))

(defun prime? (n)
  (defun smallest-divisor (n)
    (find-divisor n 2))
  (defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (+ test-divisor 1)))))
  (defun divides? (a b)
    (= (mod b a) 0))
  (= n (smallest-divisor n)))

(provide 'remainder)
(provide 'gcd)
(provide 'average)
(provide 'even?)
(provide 'odd?)
(provide 'square)
(provide 'map)
(provide 'accumulate)
(provide 'filter)
(provide 'prime?)
