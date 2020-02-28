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

;; (mod (square (expmod 3 5 26)) 26)
;; (mod (square (mod (* 3 (expmod 3 4 26)) 26)) 26)
;; (mod (square (mod (* 3 (mod (square (expmod 3 2 26)) 26)) 26)) 26)
;; (mod (square (mod (* 3 (mod (square (mod (square (expmod 3 1 26)) 26)) 26)) 26)) 26)
;; (mod (square (mod (* 3 (mod (square (mod (square (mod (* 3 (expmod 3 0 26)) 26)) 26)) 26)) 26)) 26)

;; (mod (square 10) 27)
;; (mod (square (mod 10 27)) 27)

;; (expmod 2 10 27)
;; (mod (square (expmod 2 5 27)) 27)

(defun fermat-test (n)
  (defun try-it (a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t false)))
