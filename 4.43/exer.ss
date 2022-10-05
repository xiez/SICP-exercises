;;; girl -> father
;;; melissa -> sir banacle hood
;;; mary -> moore

;;; girls: mary, gabrielle, lorna, rosalind, melissa
;;; fathers: moore, downing, hall, hood, parker


(define (puzzle)
  (let ((melissa 'sir-banacle-hood)
        (mary 'mr-moore))
    (let ((lorna (amb 'colonel-downing 'mr-hall 'dr-parker))
          (gabrielle (amb 'colonel-downing 'mr-hall 'dr-parker))
          (rosalind (amb 'colonel-downing 'mr-hall 'dr-parker)))
      (require (distinct? (list lorna gabrielle rosalind)))

      ;; Sir Barnacle’s yacht is the Gabrielle
      ;; (require (not (= gabrielle 'sir-banacle-hood)))
      
      ;; Mr. Moore owns the Lorna;
      ;; (require (not (= lorna 'mr-moore)))

      ;; Mr. Hall the Rosalind
      (require (not (= rosalind 'mr-hall)))

      ;; the Melissa, owned by Colonel Downing
      ;; (require (not (= melissa 'colonel-downing)))

      ;; Gabrielle’s father owns the yacht that is named aer Dr. Parker’s daughter.
      ;; ???

      (display (list "lorna's father is " lorna)))
    ))
