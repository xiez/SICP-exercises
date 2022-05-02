#lang sicp

(define (detect-cycle x)
  (let ((traversed '()))

    (define (set-traversed! x)
      (set! traversed (cons x traversed)))

    (define (traversed? x)
      (memq x traversed))

    (define (traverse x)
      (cond ((not (pair? x)) #f)
            ((traversed? x) (begin (display "cycle-detected!")
                                   #t))
            (else (begin
                    (set-traversed! x)
                    (or (traverse (car x))
                        (traverse (cdr x)))))))

    (traverse x)))

(define z1 (list 'a 'a 'a))             ;3
(detect-cycle z1)

(define c3 (cons 'c '()))
(set-cdr! c3 c3)
(define b3 (cons 'b c3))
(define z4 (cons 'a b3))                ;never returns
(detect-cycle z4)
