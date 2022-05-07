#lang sicp

(define (make-deque)
  ;; deque item constructor & selectors & mutators
  (define (make-item a)
    (cons a (cons '() '())))
  (define (item-content a)
    (car a))
  (define (item-prev-ptr a)
    (cadr a))
  (define (item-next-ptr a)
    (cddr a))
  (define (set-item-prev-ptr! a item)
    (set-car! (cdr a) item))
  (define (set-item-next-ptr! a item)
    (set-cdr! (cdr a) item))

  ;; deque selectors & mutators
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
          (item-content front-ptr)))

    (define (rear-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (item-content rear-ptr)))
    
    ;; mutators
    (define (rear-insert-queue! item)
      (let ((new-item (make-item item)))
        (cond ((empty-queue?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item))
              (else
               (set-item-next-ptr! rear-ptr new-item)
               (set-item-prev-ptr! new-item rear-ptr)
               (set-rear-ptr! new-item)))
        front-ptr))

    (define (front-insert-queue! item)
      (let ((new-item (make-item item)))
        (cond ((empty-queue?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item))
              (else
               (set-item-next-ptr! new-item front-ptr)
               (set-item-prev-ptr! front-ptr new-item)
               (set-front-ptr! new-item)))
        front-ptr))

    (define (front-delete-queue!)
      (if (empty-queue?) (error "DELETE called with an empty queue")
          (let ((next-item (item-next-ptr front-ptr))
                (content-deleted (item-content front-ptr)))
            (set-item-prev-ptr! next-item '())
            (set-item-next-ptr! front-ptr '())
            (set-front-ptr! next-item)
            content-deleted)))

    (define (rear-delete-queue!)
      (if (empty-queue?) (error "DELETE called with an empty queue")
          (let ((pre-item (item-prev-ptr rear-ptr))
                (content-deleted (item-content rear-ptr)))
            (set-item-next-ptr! pre-item '())
            (set-item-prev-ptr! rear-ptr '())
            (set-rear-ptr! pre-item)
            content-deleted)))
    
    (define (print)
      (define (loop ptr)
        (if (null? ptr) '()
            (cons (item-content ptr) (loop (item-next-ptr ptr)))))
      (loop front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) empty-queue?)
            ((eq? m 'front-deque) front-queue)
            ((eq? m 'rear-deque) rear-queue)
            ((eq? m 'front-insert-deque!) front-insert-queue!)
            ((eq? m 'rear-insert-deque!) rear-insert-queue!)
            ((eq? m 'print-queue) print)
            ((eq? m 'front-delete-deque!) front-delete-queue!)
            ((eq? m 'rear-delete-deque!) rear-delete-queue!)
            (else (error "undefined operation -- QUEUE" m))))
    dispatch))

;;; test
(define q1 (make-deque))
((q1 'front-insert-deque!) 'b)
((q1 'front-insert-deque!) 'a)
((q1 'rear-insert-deque!) 'c)
((q1 'print-queue))

((q1 'front-delete-deque!))
((q1 'print-queue))
((q1 'rear-delete-deque!))
((q1 'print-queue))

((q1 'front-deque))
((q1 'rear-deque))
