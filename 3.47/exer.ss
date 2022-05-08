#lang sicp

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

;;; use mutex ====================
(define (make-semaphore n)
  (let ((counter 0)
        (mutex (make-mutex)))

    (define (the-sem m)
      (cond ((eq? m 'acquire)
             (begin
               (mutex 'acquire)
               (if (< counter n)
                   (begin
                     (set! cell (+ counter 1))
                     (mutex 'release))

                   (begin
                     (mutex 'release)
                     (the-sem 'acquire)))))
            ((eq? m 'release)
             (begin
               (mutex 'acquire)
               (set! counter (- counter 1))
               (mutex 'release)))))
    the-sem))

;;; use test-and-set! ====================
(define (make-semaphore n)
  (let ((counter 0)
        (cell (list false)))

    (define (the-sem m)
      (cond ((eq? m 'acquire)
             (begin
               (if (test-and-set! cell)
                   (the-sem 'acquire)   ; the lock is held by others, try next round
                   (if (< counter n)    ; get the lock, start mutate the counter
                       (begin
                         (set! counter (+ counter 1))
                         (clear! cell)  ; release the lock
                         )

                       (begin
                         (clear! cell) ;release the lock
                         (the-sem 'acquire)))
                   )
               ))
            ((eq? m 'release)
             (begin
               (if (test-and-set! cell)
                   (the-sem 'release)   ; the lock is held by others, try next round
                   (begin               ;get the lock
                     (set! counter (- counter 1))
                     (clear! cell)      ;release the lock
                     ))))))
    the-sem))
