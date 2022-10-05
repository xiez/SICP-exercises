(define (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (require
     (or
      (and (= kitty 2) (not (= betty 3)))
      (and (= betty 3) (not (= kitty 2)))))
    (require
     (or
      (and (= ethel 1) (not (= joan 2)))
      (and (= joan 2) (not (= ethel 1)))))
    (require
     (or
      (and (= joan 3) (not (= ethel 5)))
      (and (= ethel 5) (not (= joan 3)))))
    (require
     (or
      (and (= kitty 2) (not (= mary 4)))
      (and (= mary 4) (not (= kitty 2)))))
    (require
     (or
      (and (= mary 4) (not (= betty 1)))
      (and (= betty 1) (not (= mary 4)))))
    (list (list 'betty betty) (list 'ethel ethel)
          (list 'joan joan) (list 'kitty kitty)
          (list 'mary mary))
    ))
