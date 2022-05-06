#lang sicp

(define (last-item lst) 
   (if (zero? (length (cdr lst))) 
      (car lst) 
      (last-item (cdr lst))))

(define (all-but-last lst)
  (reverse (cdr (reverse lst))))

(define (make-table)
  (define (new-record keys v)
    (cons keys v))
  (define (update-record! record v)
    (set-cdr! record v))
  (define (record-value record)
    (cdr record))

  (let ((local-table (list '*table*)))
    (define (find-record keys records)
      (define (find keys record)
        (if (null? record) false
            (equal? keys (car record))))

      (cond ((not (pair? records)) false)
            ((find keys (car records)) (car records))
            (else (find keys (cdr records)))))

    (define (lookup keys)
      (let ((record (find-record keys (cdr local-table))))
        (if record
            (record-value record)
            false)))
    (define (insert! keys value)
      (let ((record (find-record keys (cdr local-table))))
        (if record
            (update-record! record value)
            (set-cdr! local-table
                      (cons (new-record keys value)
                            '()))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; test
(define t (make-table))
(define get
  (lambda args
    ((t 'lookup-proc) args)))
(define put
  (lambda args
    (let ((val (last-item args))
          (keys (all-but-last args)))
      ((t 'insert-proc!) keys val))))


(put 'k1 'k2 1)
(get 'k1 'k2)
(get 'k1 'k2 'k3)
(put 'k1 'k2 'k3 2)
(get 'k1 'k2 'k3)

(put 'math '+ 43)
(get 'math '+)
(get 'math)

