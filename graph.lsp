(defun dot-name (exp)
"location 3063"
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defParameter *max-label-length* 12)
(defun dot-label (exp)
"location 3115"
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

(defun nodes->dot (nodes)
"location 3129"
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label (car node)))
	  (princ "\"];"))
	  nodes))

(defun edges->dot (edges)
"location 3166"
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
;the next lines CDDR was originally CDR, making for glutinous edge label
		  (princ (dot-label (cddr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
"location 3182"
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
"location 3199:"
  (with-open-file (*standard-output* fname
				     :direction :output
				     :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
"location 3337"
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))

(defun uedges->dot (edges)
"location 3371"
  (maplist (lambda (lst)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge) (cdr lst))
		       (fresh-line)
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
;changed cdr to cddr for cleaner edge label
		       (princ "[label=\"")
		       (princ (dot-label (cddr edge)))
		       (princ "\"];")
		       ))
		   (cdar lst)))
	   edges))

(defun ugraph->dot (nodes edges)
"location 3371"
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
"location 3371"
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))