;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(spade diamond heart clover))
(defconstant +numbers+ '(1 2 3 4 5 6 7 8 9 10 J Q K))
(defun generate (remained-cards &optional (generated nil) (permutation nil))
  (cond ((eq remained-cards nil) generated)
	((eq generated nil)
	 (generate (cdr remained-cards)
		   (mapcar #'(lambda (r)
			       (list r))
			   (car remained-cards))
		   permutation))
	(t
	 (generate (cdr remained-cards)
		   (mapcan #'(lambda (g)
			       (mapcar #'(lambda (r)
					   (cons r g))

				       (if permutation
					   (set-difference (car remained-cards) g :test #'equal)
					   (car remained-cards))
				       
				       ))
			   generated)
		   permutation))))

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
			  (print (car s))
			  (let ((s1 (generate (car s) nil t)))
			    (dolist (l s1)
			      (format fs "~{~a ~}~%" l))
			    (close fs))
			  (inner (cdr s) (+ n 1)))))))
	(inner suits)
	'DONE))))
