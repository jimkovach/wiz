(load "graph.lsp")

(defun known-nodes()
"location 3848"
(mapcar (lambda (node)
	  (if (member node *visited-nodes*)
	      (let ((n (assoc node *wizard-nodes*)))
		(if (eql node *location*)
		    (append n '(*))
		  n))
;	    (list node '?)
))
	(remove-duplicates
;	 (append *visited-nodes*
;		 (mapcan (lambda (node)
;			   (mapcar #'car
;				   (cdr (assoc node
;					       *wizard-edges*))))
			 *visited-nodes*)))

(defun known-edges()
"location 3868"
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				   (list (car x))))
			       (cdr (assoc node *wizard-edges*)))))
	  *visited-nodes*))

(defun draw-known ()
"location 3904"
  (ugraph->png "wizards.known.dot" (known-nodes) (known-edges)))