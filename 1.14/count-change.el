(defun count-change (amount)
  (cc amount 5))

(defun cc (amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        (t (if (<= kinds-of-coins 0) 0
                  (+ (cc amount (- kinds-of-coins 1))
                     (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))))

(defun first-denomination (kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)
