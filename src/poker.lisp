;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(s d h c))
(defconstant +numbers+ '(01 02 03 04 05 06 07 08 09 10 11 12 13))
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
	     ;; (if (eq post-proc #'pproc2) (print states))
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
						      ;; (intern (format nil "~a~a" c n)))
						  (intern (format nil "~a~2,'0D" c n)))
						  numbers))
				      (car combination-suits)) ; (c c c c c)
			      generated)))))

;; sorted suits
(defun sort-suits (suits)
  (labels ((inner (new sorted &optional passed)
	     (cond ((null sorted) (append passed (list new)))
		   ((string-lessp new (car sorted)) (append passed (cons new sorted)))
		   (t (inner new (cdr sorted) (append passed (cons (car sorted) nil)))))))
    (inner (car suits) (cdr suits))))

(defun temp (possible)
  (labels ((succ (n p)
	     (cond ((= n 0) (car p))
		   (t (succ (- n 1) (cdr p))))))
    (lambda (suits)
      (let ((num-in-hands (length suits)))
	(cond ((= +number-in-hand+ num-in-hands) nil)
	      (t (let ((p (succ num-in-hands possible)))
		   ;; (print suits)
		   ;; (print (set-difference p suits))
		   (let ((r (mapcar #'(lambda (p1) ;; (cons p1 suits)
					(sort-suits (cons p1 suits))
					)
				    (set-difference p suits))))

		     r))))))))

(defun pproc2 (in-hands args)
  (cond ((= +number-in-hand+ (length in-hands))
	 (if (not (funcall (funcall (cadr args) 'lookup) in-hands))
	     (funcall (funcall (car args) 'insert) '(acc) in-hands))
	 (funcall (funcall (cadr args) 'insert) in-hands in-hands)
	   args)
	 (t args)))

(defun test ()
  (let ((combi-suits (cdr (assoc 'acc (funcall
				       (funcall (car (generate '((s) (d) (h) (c))
							       #'null
							       #'car
							       (in-hands +suits+)
							       #'append
							       #'pproc
							       (list
								(make-table :name 'accs)
								(make-table :name 'freq))))
						'records))))))
    (let ((suits (append-number combi-suits +numbers+)))
      (labels ((inner (s &optional (n 1) (cumul 0))
		 ;; (let ((fs (open (concatenate 'string "./test" (format nil "~a" n) ".txt")
		 ;; 		 :direction :output
		 ;; 		 :if-exists :supersede
		 ;; 		 :if-does-not-exist :create)))
		   (cond ((null s) cumul)
			 (t
			  (let ((temp-suits (car s))
				(cum 0))
			    ;; (print temp-suits)
			    (let ((cards-at-first (funcall (in-hands (car temp-suits)) nil)))
			      ;; (print cards-at-first)
			      (let ((generated (generate cards-at-first
							    #'null
							    #'car
							    (temp temp-suits)
							    #'append #'pproc2
							    (list
							     (make-table :name 'accs)
							     (make-table :name 'freq
									 :proc #'(lambda (v rv)
										   (declare (ignore rv))
										   v))))))
				(let ((r (cdr (assoc 'acc (funcall (funcall (car generated) 'records))))))
				  (dolist (x r)
				    (setf cum (+ (length x) cum)))
				;;   (format fs "~{~a ~}~%" x))
				;; (close fs)
				)))
			  (inner (cdr s) (+ n 1) (+ cumul cum)))))))
	(inner suits)))))
