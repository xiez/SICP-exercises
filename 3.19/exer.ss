#lang sicp

;;; Floyd's cycle-finding algorithm
;;; https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare

(define (detect-cycle x)
  (let ((tortoise x)
        (hare (cdr x)))

    (define (meet?)
      (eq? tortoise hare))

    (define (loop)
      (cond ((null? hare) #f)
            ((null? (cdr hare)) #f)
            ((meet?) #t)
            (else (begin (set! tortoise (cdr tortoise))
                         (set! hare (cddr hare))
                         (loop)))))

    (loop)))

(define z1 (list 'a 'a 'a))             ;3
(detect-cycle z1)

(define c3 (cons 'c '()))
(set-cdr! c3 c3)
(define b3 (cons 'b c3))
(define z4 (cons 'a b3))                ;never returns
(detect-cycle z4)
