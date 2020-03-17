(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (t
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(defun no-more? (coin-values)
  (<= (length coin-values) 0))

(defun except-first-denomination (coin-values)
  (cdr coin-values))

(defun first-denomination (coin-values)
  (car coin-values))


;; ****************************************
(setq us-coins (list 50 25 10 5 1))
(setq us-coins (reverse (list 50 25 10 5 1)))
(setq uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)

