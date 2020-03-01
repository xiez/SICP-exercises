;; https://www.emacswiki.org/emacs/LexicalBinding
(setq lexical-binding t)
;; (setq test (let ((foo "bar"))
;; 	     (lambda () 
;;                foo)))

;; (funcall test)

;; (let ((foo "something-else"))
;;   (funcall test))

(defun sum (term a next b)
    (if (> a b)
        0
        (+ (funcall term a)
           (sum term (funcall next a) next b))))

(defun simpson (f a b n)
  (defun even? (n) (= (mod n 2) 0))

  (let ((h (/ (float (- b a)) n)))

    (defun simpson-term (k)
      (* (funcall f (+ a (* k h)))
         (cond ((or (= k 0) (= k n)) 1)
               ((even? k) 2)
               (t 4))))

    (* (/ h 3)
       (sum 'simpson-term 0 '1+ n))))

(defun cube (n)
  (* n n n))

(simpson 'cube 0 1 100)
(simpson 'cube 0 1 1000)

