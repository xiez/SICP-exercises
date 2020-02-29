(defun smallest-divisor (n)
  (defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (next test-divisor)))))
  (defun divides? (a b)
    (= (mod b a) 0))
  (defun square (n)
    (* n n))
  (defun next (test-divisor)
    (if (= test-divisor 2)
        3
      (+ test-divisor 2)))
  (find-divisor n 2))

(defun prime? (n)
  (defun square (n)
    (* n n))
  (defun smallest-divisor (n)
    (find-divisor n 2))
  (defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (+ test-divisor 1)))))
  (defun divides? (a b)
    (= (mod b a) 0))
  (= n (smallest-divisor n)))

;; (dolist (num '(199 1999 19999))
;;   (message
;;           "Smallest divisor of %d is %d." num (smallest-divisor num)))

(defun timed-prime-test (n)
  (message "")
  (princ n)
  (start-prime-test n (float-time)))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (report-prime (- (float-time) start-time))
      nil))

(defun report-prime (elapsed-time)
  (princ " *** ")
  (princ elapsed-time))

;;
(defun search-for-primes (n count)
  (defun iter (n count)
    (cond ((< count 1) 0)
          ((timed-prime-test n) (search-for-primes (+ n 1) (- count 1)))
          (t (search-for-primes (+ n 1) count))))
  (iter n count))

(search-for-primes 1001 3)              ;3.504753112792969e-050
(search-for-primes 10001 3)             ;8.511543273925781e-050
(search-for-primes 100001 3)            ;0.00026
(search-for-primes 1000001 3)           ;0.00077
