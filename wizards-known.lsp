(load "graph.lsp")

(defun known-nodes()
"location 3848"
(mapcar (lambda (node)
	  (if (member node *visited-nodes*)
	      (let ((n (assoc node *wizard-nodes*)))
		(if (eql node *location*)
		    (append n '(*))
		  n))
	    (list node '?)))
	(remove-duplicates
	 (append *visited-nodes*
		 (mapcan (lambda (node)
			   (mapcar #'car
				   (cdr (assoc node
					       *wizard-edges*))))
			 *visited-nodes*)))))