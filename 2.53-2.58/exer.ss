(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; 2.53
(list 'a 'b 'c)
('a 'b 'c)

(list (list 'george))
(('george))

(cdr '((x1 x2) (y1 y2)))
(y1 y2)

(cadr '((x1 x2) (y1 y2)))
(y1 y2)

(pair? (car '(a short list)))
#f

(memq 'red '((red shoes) (blue socks)))
#f

(memq 'red '(red shoes blue socks))
(red shoes blue socks)

;;; 2.54
(define (equal? l1 l2)
  (cond ((and (symbol? l1) (symbol? l2)) (eq? l1 l2))
        ((symbol? l1) (symbol? l2))
        ((symbol? l2) (symbol? l1))
        ((null? l1) (null? l2))
        ((null? l1) (null? l2))
        (else (and (equal? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2))))))

;;; 2.55

''abc equals (quote (quote abc)), thus (car ''abc) -> quote
