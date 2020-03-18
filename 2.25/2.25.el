(setq lst1 (list 1 3 (list 5 7) 9))


(car (cdr (car (cdr (cdr lst1)))))


(setq lst2 (list (list 7)))

(car (car lst2))

(setq lst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cadr (cadr (cadr (cadr (cadr (cadr lst3))))))


