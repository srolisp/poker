;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(s d h c))
(defconstant +numbers+ '(1 2 3 4 5 6 7 8 9 10 J Q K))
(defconstant +number-in-hand+ 5)

(defun equal-cards (c1 c2)
  (cond ((and (null c1) (null c2)))))

(defun setize (l1 &optional acc)
  (assert (not (null acc)))
  (cond ((null l1) acc)
	(t (let ((f (find (car l1)
			  acc ; set
			  :test #'(lambda (i1 i2) ; (..), (..)
				    (null (set-difference i1 i2 :test #'equal))))))
	     (format t ":: ~a ~a ~a ~%" (car l1) acc f)
	     (cond ((null f) (setize (cdr l1) (cons (car l1) acc)))
		   (t (setize (cdr l1) acc)))))))

(defun custom-p (proc generated &optional new-generated)
  (cond ((null generated) new-generated)
	(t (let ((applied (funcall proc (car generated)))) ; ((D ..) .. (C ..))
	     (cond ((null new-generated)
		    (custom-p proc
			      (cdr generated)
			      applied))
		   (t
		    (custom-p proc
			      (cdr generated)
			      (setize applied new-generated) ; ((D ..) .. (C ..)), ((D ..) .. (C ..))
			      )))))))

(defun p (remained-cards generated)
  (cond ((null generated)
	 (mapcar #'(lambda (r)
		     (list r))
		 generated))
	
	(t				; ((..) (..) ..  (..)) set
	 (custom-p #'(lambda (g)		;(..)
		     (mapcar #'(lambda (r) ; C D H S => ((D ..) .. (C ..)) not set with duplicates
				 (cons r g))
			     remained-cards))
		 generated))))

; (generate +suits+ #'car #'(lambda (s) +suits+) #'(lambda (cc s &optional (n 0))
					; (cond ((= n 5) s)
					;       (t ))))

(defun suit-table ()
  (let ((table (make-table :proc #'(lambda (v rv) (declare (ignore rv)) v))))
    (funcall (funcall table 'insert) '(S) 0)
    (funcall (funcall table 'insert) '(D) 0)
    (funcall (funcall table 'insert) '(H) 0)
    (funcall (funcall table 'insert) '(C) 0)
  table))

(defun generate (suits select candidate merge post-proc &optional
							  (stable (suit-table))
							  (table (make-table))
							  (selected-cards nil)
							  (acc nil))
  (format t "~a ~a ~a " suits selected-cards acc)
  (cond ((null suits) acc)
	((= (length selected-cards) +number-in-hand+)
	 (let ((records (funcall (funcall stable 'records))))
	   (let ((freq (mapcar #'(lambda (r) (cdr r)) records))) ; (3 1 1)
	     (let ((found (funcall (funcall table 'lookup) freq))) ; TODO: case? and result?
	       (format t "~a ~a ~%" freq found)
	       (let ((new-acc nil))
		 ;; if found, it means that suits already is maded.
		 (cond (found (setf new-acc acc))
		       (t (setf new-acc (cons selected-cards acc))
			  (funcall (funcall table 'insert) freq selected-cards))) ; update table
		 ;; reset
		 (map nil #'(lambda (r)
			      (funcall (funcall stable 'insert) (list (car r)) 0))
		      records)
		 (generate suits
			   select
			   candidate
			   merge
			   post-proc
			   stable
			   table
			   (cdr selected-cards)
			   new-acc))))))
	
	(t (print "in (length < +number-in-hand+)")
	   (let ((selected-card (funcall select suits)))
	     (let ((candidate-cards (funcall candidate selected-card))
		   (in-hands (cons selected-card selected-cards)))
	       (funcall post-proc selected-card stable)
	       (if (= (length in-hands) +number-in-hand+)
		   (generate (cdr suits)
			 select
			 candidate
			 merge
			 post-proc
			 stable
			 table
			 in-hands
			 acc)
		   (generate (funcall merge candidate-cards (cdr suits))
			 select
			 candidate
			 merge
			 post-proc
			 stable
			 table
			 in-hands
			 acc)))))))






;; (generate +suits+
;; 		 #'car
;; 		 #'(lambda (s) +suits+)
;; 		 #'(lambda (cc remained-suits)
;; 		     (append cc remained-suits))
;; 		 #'(lambda (sc scs s-t)		; need stable initialize before processing.
;; 		     (let ((found (funcall (funcall s-t 'lookup) (list sc))))
;; 		       (if found
;; 			   (funcall (funcall s-t 'insert) (list sc) (+ 1 found))
;; 			   (funcall (funcall s-t 'insert) (list sc) 1)))))


;; (defun generate (remained-cards &optional (generated nil) (permutation nil))
;;   (cond ((eq remained-cards nil) generated)
;; 	((eq generated nil)
;; 	 (generate (cdr remained-cards)
;; 		   (mapcar #'(lambda (r)
;; 			       (list r))
;; 			   (car remained-cards))
;; 		   permutation))
;; 	(t
;; 	 (generate (cdr remained-cards)
;; 		   (mapcan #'(lambda (g)
;; 			       (mapcar #'(lambda (r)
;; 					   (cons r g))

;; 				       (if permutation
;; 					   (set-difference (car remained-cards) g :test #'equal)
;; 					   (car remained-cards))
				       
;; 				       ))
;; 			   generated)
;; 		   permutation))))


(defun append-number (combination-suits numbers &optional (generated nil))
  (cond ((eq combination-suits nil) generated)
	(t
	 (append-number (cdr combination-suits)
			numbers
			(cons (mapcar #'(lambda (c) ; c
					  (mapcar #'(lambda (n)
						      (format nil "~a~a" c n))
						  numbers))
				      (car combination-suits)) ; (c c c c c)
			      generated)))))


(defun test ()
  
  (let ((combi-suits (generate '((c d h s)
				  (c d h s)
				  (c d h s)
				  (c d h s)
				  (c d h s)))))
    (let ((suits (append-number combi-suits +numbers+)))
      (labels ((inner (s &optional (n 1))
		 (let ((fs (open (concatenate 'string "./test" (format nil "~a" n) ".txt")
				 :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)))
		   (cond ((null s) nil)
			 (t
			  (let ((s1 (generate (car s) nil t)))
			    (dolist (l (remove-duplicates s1
					       :test #'(lambda(l1 l2)
							 (null (set-difference l1 l2 :test #'equal)))))

			      (format fs "~{~a ~}~%" l))
			    (close fs))
			  (inner (cdr s) (+ n 1)))))))
	(inner suits)
	'DONE))))
