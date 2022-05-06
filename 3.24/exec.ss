#lang sicp

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (car (car records))) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;;; test
(define t (make-table
           (lambda (x y)
             (or (equal? x y)
                 (< (abs (- x y)) 0.1)))))

(define t (make-table
           (lambda (x y)
             (if (and (number? x) (number? y))
                 (< (abs (- x y)) 0.1)
                 (equal? x y)))))

((t 'insert-proc!) 'math '+ 43)
((t 'insert-proc!) 1 1 'hello)
((t 'lookup-proc) 1 1)
((t 'lookup-proc) 1.01 1)
((t 'lookup-proc) 'math '+)
((t 'lookup-proc) 'math '-)



