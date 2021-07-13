;;;; poker.lisp

(in-package :poker)

(defconstant +suits+ '(spade diamond heart clover))
(defconstant +number-each-suit+ 13)
(defconstant +numbers+ '(1 2 3 4 5 6 7 8 9 10 J Q K))
;;((s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 sj sq sk) (c1  c2 c3 c4 c5 c6 c7 c8 c9 c10 cj cq ck) (d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 dj dq dk) (h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 hj hq hk))
;; ((1 2 .. K)..5)
(defun generate (remained-cards &optional (generated nil))
  (cond ((eq remained-cards nil) generated)
	((eq generated nil)
	 (generate (cdr remained-cards)
		   (mapcar #'(lambda (r)
			       (list r))
			   (car remained-cards))))
	(t
	 (generate (cdr remained-cards)
		   (mapcan #'(lambda (r)
			       (mapcar #'(lambda (g)
					   (cons r g))
				       generated))
			   (car remained-cards))))))

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


(defun generate-combinations ()
  (let ((combi-suits (generate '((c d h s)
				 (c d h s)
				 (c d h s)
				 (c d h s)
				 (c d h s)))))
    (let ((suits (append-number combi-suits +numbers+)))
      (labels ((inner (s &optional (acc nil))
		 (cond ((null s)
			acc)
		       (t
			(inner (cdr s) (cons (generate (car s)) acc))))))
	(inner suits)))))

(defun test ()
  (with-open-file (fs "./test.txt"
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create)
    (let ((combi-suits (generate '((c d h s)
				 (c d h s)
				 (c d h s)
				 (c d h s)
				 (c d h s)))))
      (let ((suits (append-number combi-suits +numbers+)))
	(labels ((inner (s &optional (acc nil))
		   (cond ((null s)
			  acc)
			 (t
			  (inner (cdr s) (cons (generate (car s)) acc))))))
	  (let ((s (inner suits)))
	    (dolist (l s)
	      (format fs "~a~%" l))))))))
