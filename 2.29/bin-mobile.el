(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))


;; selectors ******************************

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (car (cdr mobile)))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (car (cdr branch)))

(defun simple-weight-structure? (structure)
  (if (consp structure)
      nil
    t))

;; ****************************************
(defun branch-total-weight (branch)
  (let ((s (branch-structure branch)))
    (cond ((null branch) 0)
          ((simple-weight-structure? s)
           s)
          (t (total-weight s)))))

(defun total-weight (mobile)
  (+ (branch-total-weight (left-branch mobile))
     (branch-total-weight (right-branch mobile))))

(setq left (make-branch 4 4))
(setq right (make-branch 4 4))
(setq mobile (make-mobile left right))

(total-weight (make-mobile (make-branch 1 mobile)
                           (make-branch 2 mobile)))


;; ****************************************
(defun branch-torque (branch)
  (let ((s (branch-structure branch)))
    (cond
     ((null branch) 0)
     ((simple-weight-structure? s) (* s (branch-length branch)))
     (t (* (mobile-torque s) (branch-length branch))))))

(defun mobile-torque (mobile)
  (+ (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(defun branch-balanced? (branch)
  (let ((s (branch-structure branch)))
    (if (simple-weight-structure? s)
        t
      (balanced? s))))

(defun balanced? (mobile)
  ;; A mobile is said to be balanced if ...

  (and
   ;; 1) the torque applied by its top-left branch is equal to that applied by
   ;; its top-right branch (that is, if the length of the left rod multiplied by
   ;; the weight hanging from that rod is equal to the corresponding product for
   ;; the right side) and if ...
   (= (branch-torque (left-branch mobile))
      (branch-torque (right-branch mobile)))

   ;; 2) each of the submobiles hanging off its branches is balanced.
   (branch-balanced? (left-branch mobile))
   (branch-balanced? (right-branch mobile))
   ))

(mobile-torque (make-mobile (make-branch 1 mobile)
                           (make-branch 2 mobile)))

(balanced? (make-mobile (make-branch 2 mobile)
                           (make-branch 2 mobile)))

;; TEST ******************************

(setq left (make-branch 4 4))
(setq right (make-branch 4 4))
(setq mobile (make-mobile left right))

(total-weight mobile)
(balanced? mobile)

(branch-structure (left-branch mobile))

(branch-length left)
