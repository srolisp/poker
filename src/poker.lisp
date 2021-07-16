;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(s d h c))
(defconstant +numbers+ '(1 2 3 4 5 6 7 8 9 10 J Q K))
(defconstant +number-in-hand+ 5)

;; states: ((C) (D) (H) (S) (C C) ....)
;;         ((C C C C C) ....(C C C) ... (D) (H) (S))
;;         ...
;;
;; nil means that all cards searched.
(defun generate (states 
		 &optional
		   (goal-p)
		   (selector)
		   (successor)
		   (merge) 	; like DFS
		   (post-proc)
		   ;; (args '((acc)))
		   (args))
  (cond ((funcall goal-p states) args)
	(t (let ((selected (funcall selector states)))
	     (generate (funcall merge
				(funcall successor selected)
				(cdr states))
		       goal-p selector successor merge post-proc
		       (funcall post-proc selected args) ; must return args
		       )))))
	   	   
;; ((eq suits 'start) (mapcar #'(lambda (s) (list s)) +suits+))
;; successor function
;; (C S ...)
;; (defun in-hands (suits &optional (possible +suits+))
;;   (cond ((= +number-in-hand+ (length suits)) nil)
;; 	(t (mapcar #'(lambda (s) (cons s suits)) possible))))
(defun in-hands (possible)
  (lambda (suits)
    (cond ((= +number-in-hand+ (length suits)) nil)
	  (t (mapcar #'(lambda (s) (cons s suits)) possible)))))

;; in-hands: (C S..)
;; args: (..)
;; return : args
(defun pproc (in-hands args)
  (cond ((= +number-in-hand+ (length in-hands))
	 (let ((freq (analyze-cards in-hands)))
	   (if (not (funcall (funcall (cadr args) 'lookup) freq))
	       (funcall (funcall (car args) 'insert) '(acc) in-hands))
	   (funcall (funcall (cadr args) 'insert) freq in-hands))
	 args)
	(t  args)))

;; return: (1 2 1 ..)
(defun analyze-cards (in-hands &optional (freq '((S . 0) (D . 0) (H . 0) (C . 0))))
  (cond ((null in-hands) (mapcar #'(lambda (kv) (cdr kv)) freq))
	(t (analyze-cards (cdr in-hands)
			  (mapcar #'(lambda (kv)
				      (if (eq (car kv) (car in-hands))
					  (cons (car kv) (+ 1 (cdr kv)))
					  kv))
				  freq)))))

(defun append-number (combination-suits numbers &optional (generated nil))
  (cond ((eq combination-suits nil) generated)
	(t
	 (append-number (cdr combination-suits)
			numbers
			(cons (mapcar #'(lambda (c) ; c
					  (mapcar #'(lambda (n)
						      (intern (format nil "~a~a" c n)))
						  numbers))
				      (car combination-suits)) ; (c c c c c)
			      generated)))))

(defun test ()
  (let ((combi-suits (cdr (assoc 'acc (funcall
				       (funcall (car (generate '((s) (d) (h) (c))
							       #'null #'car (in-hands +suits+) #'append #'pproc
							       (list (make-table :name 'accs) (make-table :name 'freq))))
						'records))))))
    ;; suits: (((S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK)
    ;;  (S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK)
    ;;  (S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK)
    ;;  (S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK)
    ;;  (S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK))
    ;; ((D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 DJ DQ DK)
    ;;  (S1 S2 S3 S4 S5 S6 S7 S8 S9 S10 SJ SQ SK)
    (let ((suits (append-number combi-suits +numbers+)))
      (let ((temp-suits (car suits)))
	(let ((cards-at-first (funcall (in-hands (car temp-suits)) nil)))
	  (generate cards-at-first
		    #'null
		    #'car
		    ))))))





