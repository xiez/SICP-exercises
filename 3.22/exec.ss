#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

        (define (set-front-ptr! item)
          (set! front-ptr item))

        (define (set-rear-ptr! item)
          (set! rear-ptr item))

        ;; selectors
        (define (empty-queue?)
          (null? front-ptr))

        (define (front-queue)
          (if (empty-queue?)
              (error "FRONT called with an empty queue")
              (car front-ptr)))

        ;; mutators
        (define (insert-queue! item)
          (let ((new-pair (cons item '())))
            (cond ((empty-queue?)
                   (set-front-ptr! new-pair)
                   (set-rear-ptr! new-pair))
                  (else
                   (set-cdr! rear-ptr new-pair)
                   (set-rear-ptr! new-pair)))
            front-ptr))

        (define (delete-queue!)
          (cond ((empty-queue?) (error "DELETE called with an empty queue"))
                (else (set-front-ptr! (cdr front-ptr))))
          front-ptr)

        (define (print)
          (define (loop ptr)
            (if (null? ptr) '()
                (cons (car ptr) (loop (cdr ptr)))))
          (loop front-ptr))

        (define (dispatch m)
          (cond ((eq? m 'empty-queue?) empty-queue?)
                ((eq? m 'front-queue) front-queue)
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'print-queue) print)
                ((eq? m 'delete-queue!) delete-queue!)
                (else (error "undefined operation -- QUEUE" m))))
        dispatch))

;;; test
(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))


