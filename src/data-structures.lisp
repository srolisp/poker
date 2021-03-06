;;;; ds.lisp
(in-package :poker)

;; table
(defun make-table (&key (name nil) (proc #'(lambda (v rv) (cons v rv))))
  (let ((local-table (list name)))
    (labels ((__insertf-kv (key value table)
	       (let ((record (assoc key (cdr table))))
		 (if record
		     (setf (cdr record) (funcall proc value (cdr record)))
		     (setf (cdr table) (cons (cons key (funcall proc value (cdr record))) (cdr table))))
		 table))
	     (__insertf-multiple-key (keys value &optional (table local-table) (old-keys nil))
	       ;; (print local-table)
	       (cond ((null keys) table)
		     ((null (cdr keys))
		      (__insertf-kv (car keys) value table)
		      (__insertf-multiple-key (cdr keys) value table (cons (car keys) old-keys)))
		     (t (let ((sub-table (assoc (car keys) (cdr table))))
			  (cond (sub-table
				 (__insertf-multiple-key (cdr keys) value sub-table (cons (car keys) old-keys)))
				(t
				 (let ((s-table (list (car keys))))
				   (setf (cdr table) (cons s-table (cdr table)))
				   (__insertf-multiple-key (cdr keys) value s-table (cons (car keys) old-keys)))))))))

	     (_insert (keys value)
	       (__insertf-multiple-key keys value)
	       #'_dispatch)
	     (_lookup (keys &optional (table local-table))
	       (cond ((null keys) (cdr table))
		     (t (let ((r (assoc (car keys) (cdr table))))
			  (cond (r (_lookup (cdr keys) r))
				(t nil))))))
	     (_records() (cdr local-table))
	     (_dispatch (m)
	       (cond ((eq m 'insert) #'_insert)
		     ((eq m 'lookup) #'_lookup)
		     ((eq m 'records) #'_records))))
      #'_dispatch)))
