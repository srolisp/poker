;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(s d h c))
(defconstant +numbers+ '(02 03 04 05 06 07 08 09 10 11 12 13 14)) ; 11 J, 12 Q, 13 K, 14 A
(defconstant +number-in-hand+ 5)

(defparameter *count* 0)

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
;; seperate suits to be searched more.
(defun combi-with (possible &optional (n 0))
  (lambda (suits args)
    (cond ((= +number-in-hand+ (length suits))
	   (let ((freq (get-frequency-cards suits)))
	     (cond ((check-already-made  freq (car args))
		    nil)
		   ;; suits : (C C C C C)
		   ;; new-possible : (C01 C02 C03 C04 C05)
		   (t
		    ;; (setf n (+ n 1))
		    (funcall (funcall (car args) 'insert) freq suits)
		    (let ((fs (open (concatenate 'string "./test" (format nil "~a" n) ".txt")
				    :direction :output
				    :if-exists :append
				    :if-does-not-exist :create))
			  (new-possible (_append-number suits +numbers+))
			  (hands (if (is-flush freq) '((flush)) nil))) ; ((C01 C02 ...)
					;  (C01 
					;  ..
					;
					;  (C01 .....  ))
		      ;; generated: args
		      ;; TODO: when (C C C C C), get all successor list ((c01 c03 c05 c06 c08) ..)
		      (let ((generated (generate (funcall (combi-with (car new-possible)) nil nil)
						 #'null
						 #'car
						 (cards-by-combi new-possible)
						 #'append
						 (memory-cards fs)
						 (append (append args (list suits) (list freq)) (list *count*) hands))))
			
			;; (format t "~a~%" generated)
			(let ((count (car (cdddr (cddr generated)))))
			  (setf *count* count)
			  (close fs)
			  nil)
			)))))
	   )
	  (t (mapcar #'(lambda (s) (cons s suits)) possible)))))

(defun pproc (in-hands args)
  ;; (declare (ignore in-hands))
  ;; TODO: if the state is (D02 D01 #01#) followed by (D01 D02 #01#).
  (cond ((= +number-in-hand+ (length in-hands))
	 ))
  args)

;; travel all possible suits. 
(defun cards-by-combi (possible)
  (labels ((succ (n p)
	     (cond ((= n 0) (car p))
		   (t (succ (- n 1) (cdr p))))))
    ;; args: freq table, unique check table, pattern table, combi, freq, '(flush) or nil.
    (lambda (suits args)
      (let ((num-in-hands (length suits)))
	(let ((p (succ num-in-hands possible)))	   ; get the possible row.
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

;; post-proc
;; must return args
;; suits (C01 C02 C03 C04 .. )

(defun memory-cards (fs)
  (lambda (suits args)
    ;; usable table1 is for classifying unique suits to prevent to be duplicated.
    (cond ((= +number-in-hand+ (length suits))
	   (cond ((not (check-already-made suits (cadr args)))
		  ;; (3 2) .. (5) .. (1 1 1 1 1) .....
		  (multiple-value-bind (pn pn-high) (pattern-number #'(lambda (s1 s2)
									(string= (subseq (symbol-name s1) 1)
										 (subseq (symbol-name s2) 1)))
								    suits) ;suits is sorted.
		    ;; pn: (2 1 1 1), pn-high: 3
		    (let ((r (funcall (funcall (caddr args) 'lookup) pn))
			  (flush (cadddr (cdddr args)))) ; get hands according pn.
		      (let ((count (car (cdddr (cddr args)))))
			(setf count (+ count 1))
			(setf (car (cdddr (cddr args))) count)
			
		      
		      (format fs "~a " count)
		      ;; r: 2-pair
		      (cond ((eq r '2cases)	; high card or straight
			     (multiple-value-bind (hands high) (is-straight (to-int-list suits))
			       (cond ((and flush hands (= high 14))
				      (funcall (funcall (cadr args) 'insert) suits
					       (cons pn-high (cons 'royal (cons hands flush))))
				      (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high (cons 'royal (cons hands flush))))
				     ((and flush hands)
				      (funcall (funcall (cadr args) 'insert) suits
					       (cons pn-high (cons hands flush)))
				     (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high (cons hands flush)))
				     (flush	; just flush
				      (funcall (funcall (cadr args) 'insert) suits
					       (cons pn-high flush))
				      (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high flush))
				     (hands	; jsut hands
				      (funcall (funcall (cadr args) 'insert) suits
					       (cons pn-high (list hands)))
				      (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high (list hands)))
				     (t
				      (funcall (funcall (cadr args) 'insert) suits
					       (cons pn-high nil))
				      (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high nil))
				     )))
			    (t		; one pair, two pair, three, four, fl.
			     (funcall (funcall (cadr args) 'insert) suits
				      (cons pn-high (list r)))
			     (format fs "~{~a ~}~a~{~a ~}~%" suits pn-high (list r))))
		      ))))
		 (t )))
	  (t ))
    args))

(defun get-hands-values (numbers pn-hands)
  (cond ((eq pn-hands '2cases)
	 )))

(defun to-int-list (suits)
  (mapcar #'(lambda (s)
	      (parse-integer (subseq (symbol-name s) 1)))
	  suits))

(defun is-straight (numbers &optional (prev 0) (high 0))
  ;; pn: (1 1 1 1 1)
  (cond ((null numbers) (values 'straight high))
	((= prev 0) (is-straight (cdr numbers) (car numbers) (car numbers)))
	((= prev (+ (car numbers) 1)) (is-straight (cdr numbers) (car numbers) high))
	(t (values nil high))))

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
(defun pattern-number (func sorted-suits &optional acc (acc2 1) (high (car sorted-suits)) (max nil))
  (labels ((inner (item remained)
	     (cond ((null item) (pattern-number func nil acc acc2 high max))
		   (t (let ((p (funcall func item (car remained))))
			(cond ((and p (null max))
			       (pattern-number func (cons item (cdr remained)) acc (+ acc2 1) high item))
			      (p (pattern-number func (cons item (cdr remained)) acc (+ acc2 1) high max))
			      (t (pattern-number func remained (append acc (list acc2)) 1 high max))))))))
    (cond ((null sorted-suits) (if max (values acc (parse-integer (subseq (symbol-name max) 1)))
				   (values acc (parse-integer (subseq (symbol-name high) 1)))))
	  (t (inner (car sorted-suits) (cdr sorted-suits))))))

;; possible is not sorted.


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
  (let ((table (make-table :name 'pattern :proc #'(lambda (v rv) (declare (ignore rv)) v))))
    ;; TODO: rank like (1 . four) 
    (funcall (funcall table 'insert) '(1 4) 'four)
    (funcall (funcall table 'insert) '(4 1) 'four)
    (funcall (funcall table 'insert) '(2 3) 'fullhouse)
    (funcall (funcall table 'insert) '(3 2) 'fullhouse)
    (funcall (funcall table 'insert) '(1 1 3) 'three)
    (funcall (funcall table 'insert) '(1 3 1) 'three)
    (funcall (funcall table 'insert) '(3 1 1) 'three)
    (funcall (funcall table 'insert) '(1 2 2) '2-pair)
    (funcall (funcall table 'insert) '(2 1 2) '2-pair)
    (funcall (funcall table 'insert) '(2 2 1) '2-pair)
    (funcall (funcall table 'insert) '(1 1 1 2) '1-pair)
    (funcall (funcall table 'insert) '(1 1 2 1) '1-pair)
    (funcall (funcall table 'insert) '(1 2 1 1) '1-pair)
    (funcall (funcall table 'insert) '(2 1 1 1) '1-pair)
    (funcall (funcall table 'insert) '(1 1 1 1 1) '2cases)

    (let ((g (generate '((s) (d) (h) (c))
		       #'null
		       #'car
		       (combi-with +suits+)
		       #'append
		       #'pproc
		       (list
			(make-table :name 'freq) 
			(make-table :name 'accs)
			table
			))))
      (rassoc '(14 royal straight flush) (funcall (funcall (cadr g) 'records))
      ))))

