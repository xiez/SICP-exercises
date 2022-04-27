(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((eq x (car set)) t)
        (t (element-of-set? x (cdr set)))))

(element-of-set? 11 '(1 2 3))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
    (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3) '(2 3 4))

;;; 2.59
(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (t (cons (car set1) (union-set (cdr set1) set2)))))
(union-set '(1 2 3) '(2 3 4))

;;; 2.63
(defun entry (tree)
  (car tree))
(defun left-branch (tree)
  (cadr tree))
(defun right-branch (tree)
  (caddr tree))
(defun make-tree (entry left right)
  (list entry left right))

(defun tree->list-1 (tree)
  (if (null tree)
      '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))
(defun tree->list-2 (tree)
  (defun copy-to-list (tree result-list)
    (if (null tree)
        result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(setq left (make-tree 2 '() '()))
(setq right (make-tree 3 '() '()))
(setq tree (make-tree 1 left right))

(tree->list-1 tree)
(tree->list-2 tree)


;;; 2.64
;;; 2.65
;;; 2.66



;;;2.67
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))
(defun leaf? (obj)
  (eq (car obj) 'leaf))
(defun symbol-leaf (x)
  (cadr x))
(defun weight-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(defun left-branch (tree)
  (car tree))
(defun right-branch (tree)
  (cadr tree))
(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
    (caddr tree)))
(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
    (cadddr tree)))

(defun decode (bits tree)
  (defun decode-1 (bits current-branch)
    (if (null bits)
        '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (t (error "bad bit --CHOOSE-BRANCH" bit))))
;;; ---------
(setq sample-tree
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                       (make-leaf 'B 2)
                       (make-code-tree (make-leaf 'D 1)
                                       (make-leaf 'C 1)))))
sample-tree
(setq sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;;; (0 1 1 0 0 1 0 1 0 1 1 1 0)
;;;  A     D A   B   B     C A

;;; 2.68
(defun encode (message tree)
  (if (null message)
      '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
(defun encode-symbol (symbol tree)
  (if (leaf? tree)
      '()
    (cond
     ((memq symbol (symbols (left-branch tree)) )
      (cons 0 (encode-symbol symbol (left-branch tree))))
     ((memq symbol (symbols (right-branch tree)) )
      (cons 1 (encode-symbol symbol (right-branch tree))))
     (t (error "bad symbol -- encode-system %s" symbol)))
    ))

(encode-symbol 'N sample-tree)

(equal
 (encode '(A D A B B C A)  sample-tree)
 '(0 1 1 0 0 1 0 1 0 1 1 1 0)
 )

;;; 2.69
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (leaf-set)
  (if (= 1 (length leaf-set) )
      (car leaf-set)
    (let ((merged (make-code-tree (car leaf-set)
                                  (cadr leaf-set))))
      (let ((new-leaf-set (adjoin-set merged (cddr leaf-set))))
           (successive-merge new-leaf-set) )
      )))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ;symbol
                             (cadr pair)) ;frequency
                  (make-leaf-set (cdr pairs))))))

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set
 (make-leaf 'A 4)
 (list (make-leaf 'B 1)) )

(setq pairs '((A 4) (B 2) (C 1) (D 1)))
(setq leaf-set (make-leaf-set pairs))
(successive-merge leaf-set)

(setq tree (generate-huffman-tree pairs))
;;; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
(equal tree sample-tree)

;;; 2.70
(setq pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(setq tree (generate-huffman-tree pairs))
(setq message
      '(GET A JOB
            SHA NA NA NA NA NA NA NA NA
            GET A JOB
            SHA NA NA NA NA NA NA NA NA
            WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
            SHA BOOM))
(length (encode message tree))          ;=> 84

(length message)                        ;=> 36
;;; fix-length encoding: 3bit /word * (length message) = 108

;;; 2.71
;;; 2.72
