#lang scheme

;;; helper functions
(define (enumerate-interval start end)
  (if (> start end) '()
      (cons start
            (enumerate-interval (+ start 1) end))))

(define (flatmap proc data)
  (accumulate append '() (map proc data)))

(define (accumulate op init seq)
  (if (null? seq) init
      (op
       (car seq)
       (accumulate op init (cdr seq)))))

;;; ----------------------------------------

(define empty-board '())

(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

;;; adjoin-position: n n (ListOf position) -> (ListOf position)
;;; cons a pair representing the position of the placed piece onto the list representing the set of board positions so far
(define (adjoin-position row col queens)
  (cons (cons row col) queens))

;;; safe?: column (ListOf (ListOf position)) -> bool
;;; determines for a set of positions, whether the queen in each row of the kth column is safe with respect to the others.
;;; (Note that we need only check whether the new queen is safe---the other queens are already guaranteed safe with respect to each other.)
(define (safe? k positions)
  ;; k is never used, since the position for kth column always appears at the beginning.
  (define (attack? posn1 posn2)
    (cond [(= (position-row posn1) (position-row posn2)) true]
          [(= (position-col posn1) (position-col posn2)) true]
          [(= (abs (- (position-row posn1) (position-row posn2)))
              (abs (- (position-col posn1) (position-col posn2)))) true]
          [else false]))
  (cond [(null? positions) true]
        [(null? (cdr positions)) true]
        [(attack? (car positions) (cadr positions)) false]
        [else (safe? k (cons (car positions) (cddr positions)))]))

;;; queens: n -> (ListOf (ListOf position))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; replace flatmap with accumulate...append...
(define (queens2 board-size)
  ;;queen-cols: n -> (ListOf (ListOf position))
  ;; e.g. ( (posn1 posn2 ...) (posn1 posn2 ...))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         ;; (flatmap
         ;;  (lambda (rest-of-queens)
         ;;    (map (lambda (new-row)
         ;;           (adjoin-position new-row k rest-of-queens))
         ;;         (enumerate-interval 1 board-size)))
         ;;  (queen-cols (- k 1)))
         (accumulate
          append
          '()
          (map
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1)))))))
  (queen-cols board-size))


;;; 2.43
;;; it will generate all the possible combinations of 8x8 positions, which is approximately
;;; 8 to the 8th power (16777216).
;;; Assuming that the program in Exercise 2-42 solves the puzzle in time T.
;;; k=1, 8 positions
;;; k=2, 64 positions, filter out 42 safe positions
;;; k=3, 42*8=336 positions, filter out 140 safe positions
;;; k=4, 140*8=1120 positions, filter out 344 safe positions
;;; the time (16777216/50)T
;;; see https://wernerdegroot.wordpress.com/2015/08/01/sicp-exercise-2-43/
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
