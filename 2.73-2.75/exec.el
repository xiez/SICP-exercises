;;; programs that use complex numbers
(defun add-complex (z1 z2))
(defun sub-complex (z1 z2))
(defun mul-complex (z1 z2))
(defun div-complex (z1 z2))

;;; complex arithmetic package
(defun real-part (z)
  (apply-generic 'real-part z))
(defun imag-part (z))
(defun magnitude (z))
(defun angle (z))

;;; reactangular representation
(defun make-from-real-imag (x y)
  ((get 'make-from-real-imag 'reactangular) x y))
;;; polar representation
(defun make-from-mag-ang (r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; tag func
(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (consp datum)
      (car datum)
    (error "Bad taggad datum")))
(defun contents (datum)
  (if (consp datum)
      (cdr datum)
    (error "Bad tagged datum")))

;;; generic func
(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar 'type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (funcall proc (map contents args))
        (error
         "No method for these types -- APPLY-GENERIC %s"
         (list op type-tags)))))
  ))

;;; 2.73
(defun real-part-polar () 'test)
(defun real-part-react (a b) 'test2)

(put 'real-part 'polar 'real-part-polar)
(put 'real-part 'rect 'real-part-react)

(funcall (get 'real-part 'polar) ) 
((get 'real-part 'rect) 1 2)

(get 'real-part 'rect)
(symbol-plist 'real-part )

;;; ------
(defun =number? (exp num) (and (numberp exp) (= exp num)))
(defun variable? (x) (symbolp x))
(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (eq v1 v2)))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

(defun deriv (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (t (funcall (get 'deriv (operator exp))
                    (operands exp) var))))

(defun install-sum-package ()
  ;; internal procedures
  (defun addend (s) (car s))
  (defun augend (s) (cadr s))
  
  (defun make-sum (a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (numberp a1) (numberp a2)) (+ a1 a2))
          (t (list '+ a1 a2))))

  (defun deriv-sum (exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ;interface to the rest of the system
  (put 'deriv '+ 'deriv-sum)

  'done)
(install-sum-package)

(defun install-product-package ()
  ;; internal procedures
  (defun multiplier (p) (car p))
  (defun multiplicand (p) (cadr p))

  (defun make-product (m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (numberp m1) (numberp m2) (* m1 m2)))
          (t (list '* m1 m2))))

  (defun deriv-product (exp var)
    (make-sum
     (make-product (multiplier exp) (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var) (multiplicand exp))))

  ;; interface to the rest of the system
  (put 'deriv '* 'deriv-product)

  'done)
(install-product-package)

(defun install-exponent-package ()
  (defun base (x) (car x))
  (defun exponent (x) (cadr x))
  (defun make-exponentation (base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          ((and (numberp base) (numberp exp)) (expt base exp))
          (t (list '** base exp))))

  (defun deriv-exp (exp var)
    (make-product
          (make-product
           (exponent exp)
           (make-exponentation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '** 'deriv-exp)

  'done)
(install-exponent-package)

(put '+ 'deriv 'test)
(put '* 'deriv 'test2)
(get '+ 'deriv)

;;; 2.74
;;; build a table similar to the operations-types table for the complex system
;;;            | division 001 | division 002 | ...
;;; get-record | get-record-1 | get-record-2 | ...
;;; get-salary | get-salary-1 | get-salary-2 | ...


(defun install-div1-package ()
  (defun employee? (r)
    (consp r))

  (defun employee-name (r)
    (cadr (cadr r)))

  (defun employee-salary (r)
    (cadr (caddr r)))

  (defun get-record-1 (employee file)
    (cond ((null file) '())
          ((not (employee? (car file))) (get-record-1 employee (cdr file)))
          ((eq employee (employee-name (car file))) (car file))
          (t (get-record-1 employee (cdr file)))
          ))

  (defun get-salary-1 (employee file)
    (let ((rec (get-record-1 employee file)))
      (if (null rec) '()
        (employee-salary rec))))

  ;; interface
  (put 'get-record 'division-1 'get-record-1)
  (put 'get-salary 'division-1 'get-salary-1)
  'done)
(install-div1-package)


(defun get-record (employee file)
  (funcall (get 'get-record (car file)) employee file))

(defun get-salary (employee file)
  (funcall (get 'get-salary (car file)) employee file))

(defun find-employee-record (employee files)
  (if (null files) '()
      (let ((rec (get-record employee (car files))))
        (if (null rec)
            (find-employee-record employee (cdr files))
          rec))))

;;; test
(defun make-div1-file ()
  (defun make-employee (name salary address)
    (list 'employee
          (list 'name name)
          (list 'salary salary)
          (list 'address address)))

  (list 'division-1
   (make-employee 'Bob 3000 "xx")
   (make-employee 'Alice 4000 "yy")
   (make-employee 'Scott 5000 "zz")))

(setq file1  (make-div1-file))
(get-record 'Bob file1)
(get-salary 'Alice file1)
(find-employee-record 'Bob (list file1))

;;; 2.75

(defun make-from-real-imag (x y)
  (defun dispatch (op)
    (cond ((eq op 'real-part) x)
          ((eq op 'imag-part) y)
          ((eq op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq op 'angle) (atan y x))
          (t (error "Unknown op -- MAKE-FROM-REAL-IMAG %s" op))))
  op)

(defun apply-generic (op arg)
  (arg op))
