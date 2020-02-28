(defun even? (n) (= (mod n 2) 0))
(defun square (n) (* n n))

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mod (square (expmod base (/ exp 2) m))
                    m))
        (t
         (mod (* base (expmod base (- exp 1) m))
                    m))))

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

;; ****************************************

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

(defun timed-prime-test (n)
  (message "")
  (princ n)
  (start-prime-test n (float-time)))

(defun start-prime-test (n start-time)
  (if (fast-prime? n 10)
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

(search-for-primes 1001 3)              ;0.00017
(search-for-primes 10001 3)             ;0.00019
(search-for-primes 100001 3)            ;0.00023
(search-for-primes 1000001 3)           ;0.00026
