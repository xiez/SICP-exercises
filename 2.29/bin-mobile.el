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

;; ****************************************
(defun branch-weight (branch)
  (let ((s (branch-structure branch)))
    (cond ((null branch) 0)
          ((consp s)
           (branch-weight s))
          (t s))))

(defun total-weight (mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; ****************************************
(defun branch-length (branch)
  (let ((s (branch-structure branch)))
    (cond ((null branch) 0)
          ((consp s)
           (branch-length s))
          (t (car branch)))))

(defun balanced? (mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (= (* (branch-weight left) (branch-length left))
       (* (branch-weight right) (branch-length right)))))

;; TEST ******************************

(setq left (make-branch 4 4))
(setq right (make-branch 4 4))
(setq mobile (make-mobile left right))

(total-weight mobile)
(balanced? mobile)

(branch-structure (left-branch mobile))

(branch-length left)
