(add-to-list 'load-path "..")
(require 'map "sicp")
(require 'accumulate "sicp")
(require 'filter "sicp")
(require 'prime? "sicp")


(defun enumerate-interval (start end)
  (if (> start end)
      nil
    (cons start
          (enumerate-interval (+ 1 start) end))))

(setq n 6)
(accumulate 'append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))

(defun flatmap (proc seq)
  (accumulate 'append nil (map proc seq)))

(flatmap (lambda (x) x)
         (list (list 1 2 3) (list 3 4 5)))

(flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n))

(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (map 'make-pair-sum
       (filter 'prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs n)
