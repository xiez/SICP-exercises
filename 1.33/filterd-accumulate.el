;; https://www.emacswiki.org/emacs/LexicalBinding
;; (setq lexical-binding t)

(defun odd? (n)
  (= (mod n 2) 1))

(defun filtered-accumulate (combiner null-value term a next b filter?)
  (if (> a b)
      null-value
      (funcall combiner
               (if (funcall filter? a)
                   (funcall term a)
                 null-value)
               (filtered-accumulate combiner null-value term (funcall next a) next b filter?))))

(defun sum-odd-integers (a b)
  (defun sum-odd (term a next b)
    (filtered-accumulate '+ 0 term a next b 'odd?))
  (sum-odd 'identity a '1+ b))

(sum-odd-integers 1 10)

;; ****************************************
(defun (square n)
  (* n n))

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

(defun sum-square-of-primes (a b)
  (filtered-accumulate '+ 0 'square a '1+ b 'prime?))

(sum-square-of-primes 1 10)

;; ****************************************
(defun gcd (a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(defun product-relatively-primes (n)
  (defun relatively-prime? (i)
    (= (gcd i n) 1))

  (filtered-accumulate '* 1 'identity 1 '1+ n 'relatively-prime?))

(product-relatively-primes 10)
