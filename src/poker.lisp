;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(s d h c))
(defconstant +numbers+ '(01 02 03 04 05 06 07 08 09 10 11 12 13))
(defconstant +number-in-hand+ 5)

;; states: ((S) (D) (H) (C))
;;         ((C C C C C) ....(C C C) ... (D) (H) (C))
;;         ...
;;
;; nil means that all cards searched.
(defun generate (states 
		 &optional
		   (goal-p)
		   (selector)
		   (successor)
		   (merge)     
		   (post-proc)
		   (args))
  (cond ((funcall goal-p states) args)
	(t (let ((selected (funcall selector states)))
	     (generate (funcall merge
				(funcall successor selected args)
				(cdr states))
		       goal-p selector successor merge post-proc
		       (funcall post-proc selected args) ; must return args
		       )))))
	   	   
(defun check-already-made (seq table)
  (funcall (funcall table 'lookup) seq))

;; successor function
;; (C S ...)
(defun in-hands (possible)
  (lambda (suits args)
    (cond ((= 6 (length suits)) ;TODO: 6 (f C01 C02 ..) with tag f.
	   nil)			; no successor: expanding is over.
	  ((= +number-in-hand+ (length suits))
	   (let ((freq (get-frequency-cards suits)))
	     (cond ((check-already-made  freq (car args))
		    nil)
		   ;; suits : (C C C C C)
		   ;; new-possible : (C01 C02 C03 C04 C05)
		   (t (funcall (funcall (car args) 'insert) freq suits)
		      (let ((new-possible (_append-number suits +numbers+))
			    (hands (if (is-flush freq) '((flush)) nil))) ; ((C01 C02 ...)
					;  (C01 
					;  ..
					;
					;  (C01 .....  ))
			
			;; generated: args
			;; TODO: when (C C C C C), get all successor list ((c01 c03 c05 c06 c08) ..)
			(let ((generated (generate (funcall (in-hands (car new-possible)) nil nil)
						   #'null
						   #'car
						   (temp new-possible)
						   #'append
						   #'pproc
						   (append (append args (list suits)) hands))))
			  ;; r: ((C01 C02 C03 ..) ...(C10..C13))
			  (let ((r (funcall (funcall (caddr generated) 'lookup) suits)))
			    (mapcar #'(lambda (r1)
					(cons 'f r1))
				    r)))))))
	   )
	  (t (mapcar #'(lambda (s) (cons s suits)) possible)))))

(defun is-flush (freq)
  (find-if #'(lambda (x) (= x 5)) freq))

;; in-hands: (C S..)
;; in-hands: (f C01 D02 ..)

;; return: (1 2 1 ..)
(defun get-frequency-cards (in-hands &optional (freq '((S . 0) (D . 0) (H . 0) (C . 0))))
  (cond ((null in-hands) (mapcar #'(lambda (kv) (cdr kv)) freq))
	(t (get-frequency-cards (cdr in-hands)
			  (mapcar #'(lambda (kv)
				      (if (eq (car kv) (car in-hands))
					  (cons (car kv) (+ 1 (cdr kv)))
					  kv))
				  freq)))))
;; (C C C C C)

(defun _append-number (suits numbers)
  (mapcar #'(lambda (c) ; c
	      (mapcar #'(lambda (n)
			  ;; (intern (format nil "~a~a" c n)))
			  (intern (format nil "~a~2,'0D" c n)))
		      numbers))
	  suits))

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
;; built-in function sort doesn't maintain original input.
(defun sort-suits (suits &key (key #'identity) (test #'string-lessp))
  (labels ((inner (new sorted &optional passed)
	     (cond ((null sorted) (append passed (list new)))
		   ((funcall test (funcall key new) (funcall key (car sorted))) (append passed (cons new sorted)))
		   (t (inner new (cdr sorted) (append passed (cons (car sorted) nil)))))))
    (inner (car suits) (cdr suits))))

;; sorted suits (S10 C05 C03 H03 D01)
(defun pattern-number (func sorted-suits &optional acc (acc2 1) (max (car sorted-suits)))
  (labels ((inner (item remained)
	     (cond ((null item) (pattern-number func nil acc acc2 max))
		   (t (let ((p (funcall func item (car remained))))
			(cond (p (pattern-number func (cons item (cdr remained)) acc (+ acc2 1) max))
			      (t (pattern-number func remained (append acc (list acc2)) 1 max))))))))
    (cond ((null sorted-suits) (values acc (parse-integer (subseq (symbol-name max) 1))))
	  (t (inner (car sorted-suits) (cdr sorted-suits))))))

;; possible is not sorted.
(defun temp (possible)
  (labels ((succ (n p)
	     (cond ((= n 0) (car p))
		   (t (succ (- n 1) (cdr p))))))
    (lambda (suits args)
      (declare (ignore args))
      (let ((num-in-hands (length suits)))
	(let ((p (succ num-in-hands possible)))
	  (cond ((= +number-in-hand+ num-in-hands) ; 5
		 nil)
		(t 
		 (let ((r (mapcar #'(lambda (p1) ;; (cons p1 suits)
				      ;; sorted suits (D01 C03 H03 C05 S10)
				      (sort-suits (cons p1 suits)
						  :key #'(lambda (s) (subseq (symbol-name s) 1))
						  :test #'string-greaterp))
				  (set-difference p suits))))
		   r))))))))

;; (defun temp (possible)
;;   (labels ((succ (n p)
;; 	     (cond ((= n 0) (car p))
;; 		   (t (succ (- n 1) (cdr p))))))
;;     (lambda (suits args)
;;       (let ((num-in-hands (length suits)))
;; 	(let ((p (succ num-in-hands possible)))
;; 	  (cond ((= num-in-hands (- +number-in-hand+ 1)) ; 4
;; 		 (let ((sorted nil)
;; 		       (successors nil))
;; 		   (mapc #'(lambda (p1)
;; 			     (setf sorted (sort-suits (cons p1 suits)
;; 						  :key #'(lambda (s) (subseq (symbol-name s) 1))
;; 						  :test #'string-greaterp))
;; 			     (cond ((check-already-made sorted (cadr args))
;; 				    nil)
;; 				   (t
;; 				    (funcall (funcall (cadr args) 'insert) sorted
;; 					     ;; (pattern-number #'(lambda (s1 s2)
;; 					     ;; 			 (string= (subseq (symbol-name s1) 1)
;; 					     ;; 				      (subseq (symbol-name s2) 1)))
;; 					     ;; 		     sorted)
;; 					     sorted)
;; 				    ;; (funcall (funcall (caddr args) 'insert) (cadddr args) sorted)
;; 				    (setf successors (cons sorted successors)))))
;; 			 (set-difference p suits))
;; 		   successors))
;; 		((= +number-in-hand+ num-in-hands) ; 5
;; 		 nil)
;; 		(t 
;; 		 (let ((r (mapcar #'(lambda (p1) ;; (cons p1 suits)
;; 				      ;; sorted suits (D01 C03 H03 C05 S10)
;; 				      (sort-suits (cons p1 suits)
;; 						  :key #'(lambda (s) (subseq (symbol-name s) 1))
;; 						  :test #'string-greaterp))
;; 				  (set-difference p suits))))
;; 		   r))))))))

(defun pproc (in-hands args)
  (declare (ignore in-hands))
  args)

;; sorted (C01 C02 ..)
(defun pproc2 ()
  (let ((fs (open (concatenate 'string "./test" (format nil "~a" n) ".txt")
		  :direction :output
		  :if-exists :supersede
		  :if-does-not-exist :create))
	(n 1))
    (lambda (in-hands args)
      (cond ((= +number-in-hand+ (length in-hands))
	     (format fs "~{~a ~}~%" in-hands)
	     (setf n (+ n 1)))
	    (t ))
      
      args)))

(defun pproc-with-tag (in-hands args)
  (cond ((= +number-in-hand+ (length in-hands))
	 (if (not (funcall (funcall (cadr args) 'lookup) (cons 'f in-hands)))
	     (funcall (funcall (car args) 'insert) '(acc) (cons 'f in-hands)))
	 (funcall (funcall (cadr args) 'insert) (cons 'f in-hands) (cons 'f in-hands))
	   args)
	 (t args)))

(defun test ()
  (generate '((s) (d) (h) (c))
	    #'null
	    #'car
	    (in-hands +suits+)
	    #'append
	    #'pproc
	    (list
	     (make-table :name 'freq)
	     (make-table :name 'accs)
	     (make-table :name 'accs2)
	     ))
  )

