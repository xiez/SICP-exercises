;;; helper functions
(define (cadr x)
  (car (cdr x)))

(define (cddr x)
  (cdr (cdr x)))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons
       (car lst1)
       (append (cdr lst1) lst2))))

(define (length lst)
  (if (null? lst)
      0
      (+ 1
         (length (cdr lst)))))

(define (map proc lst)
  (if (null? lst)
      null
      (cons
       (proc (car lst))
       (map proc (cdr lst)))))

(define (filter func lst)
  (if (null? lst)
      null
      (if (func (car lst))
          (cons
           (car lst)
           (filter func (cdr lst)))
          (filter func (cdr lst)))))

(define (enumerate-interval start end)
  (if (> start end) null
      (cons start
            (enumerate-interval (+ start 1) end))))

(define (accumulate op init seq)
  (if (null? seq) init
      (op
       (car seq)
       (accumulate op init (cdr seq)))))

(define (flatmap proc data)
  (accumulate append null (map proc data)))

;;; ----------------------------------------

(define empty-board null)

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
    (cond ((= (position-row posn1) (position-row posn2)) true)
          ((= (position-col posn1) (position-col posn2)) true)
          ((= (abs (- (position-row posn1) (position-row posn2)))
              (abs (- (position-col posn1) (position-col posn2)))) true)
          (else false)))
  (cond ((null? positions) true)
        ((null? (cdr positions)) true)
        ((attack? (car positions) (cadr positions)) false)
        (else (safe? k (cons (car positions) (cddr positions))))))

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

;; assertions
(=
 (map abs (list 1 -1 2 -3))
 (list 1 1 2 3)
 )


(=
 (filter (lambda (x) (> x 1)) (list 1 2 3 4 5))
 (list 2 3 4 5))


(=
 (enumerate-interval 1 5)
 (list 1 2 3 4 5))


(=
 15
 (accumulate + 0 (enumerate-interval 1 5)))


(=
 (length (queens 1))
 1
 )


(=
 (length (queens 2))
 0
 )


(=
 (length (queens 3))
 0
 )

(=
 (length (queens 4))
 2
 )

(=
 (length (queens 5))
 10
 )

(=
 (length (queens 6))
 4
 )

;; ;; (queens 8)

