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

(defun fool-fermat-test (n)
  (defun aux-test (a)
    (cond ((= a 1) t)
          ((/= (expmod a n n) a) nil)
          (t (aux-test (- a 1))))
    )
  (aux-test (- n 1)))


(fool-fermat-test 561)
(fool-fermat-test 1105)
(fool-fermat-test 1729)
(fool-fermat-test 2465)
(fool-fermat-test 2821)
(fool-fermat-test 6601)
