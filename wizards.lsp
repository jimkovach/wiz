;;;WIZARDS

(defparameter *dot-file* "/users/jimkovach/program/lisp/LoLisp/wizards/wizards.dot")
(defparameter *dot-file-known* "/users/jimkovach/program/lisp/LoLisp/wizards/wizards-known.dot")

;;LOAD GRAPH.LSP so we can use GRAFVIZ
(load "/users/jimkovach/program/lisp/LoLisp/wizards/graph.lsp")

;location 1880
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
					  A wizard is snoring loudly on the couch.))
			(kitchen (you are in a well appointed kitchen.
				      A chopping block holds one chefs knife.))
			(basement (you are in a dark - dank room.
				       Four walls surround you.))
			(entry (you are looking out at the front entry.
				    yonder may be a roadway.))
			(road (you are standing at the front road.
				     To the left-west is a mansion
				     to the right-east is just more street.))
			(garden (you are in a beautiful garden.
				     there is a well in front of you.))
			(attic (you are in the attic.
				    there is a giant welding torch in the corner.))))


(defun describe-location (location nodes)
"location 1913"
  (cadr ( assoc location nodes)))

;*wizard-edges* = pathways between rooms (*wizard-nodes*)
;location 1934
(defparameter *wizard-edges*  '((living-room (garden west door)
				     (kitchen east door)
				     (attic upstairs ladder))
			(entry (kitchen west door)
			       (road north door))
			(road (mansion west road)
				(path east road)
				(entry south door))
			(kitchen (living-room west door)
				 (entry east door)
				 (basement downstairs steps))
			(basement(kitchen upstairs steps))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

(defun describe-path (edge)
"location 1934 -- Description of the path that is being observed"
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
"location 1962"
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket knife frog chain))
;location 2082

;loation 2090
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (knife kitchen)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
"location 2090"
  (labels ((at-loc-p (obj)
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
"location 2124"
  (labels ((describe-obj (obj)
			`(you see a ,obj on the floor.)))
	 (apply #'append(mapcar #'describe-obj(objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look()
"location 2142"
  (append (describe-location *location* *wizard-nodes*)
	  (describe-paths *location* *wizard-edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(setf *visited-nodes* '(living-room))
;beginning to work on known-nodes

(defun move (direction)
"location 2171 - replaces original WALK"
  (let ((next (find direction
		    (cdr (assoc *location* *wizard-edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (pushnew *location* *visited-nodes*)
	       (look))
;(draw-known-map)
      '(you cannot go that way.))))

(defun take (object)
"location 224 - replaces original PICKUP"
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory()
"location 2266"
  (if (eq (objects-at 'body *objects* *object-locations*) nil)
      nil
  (cons 'items- (objects-at 'body *objects* *object-locations*))))

(defun game-repl()
"location 2536 - GAME-REPL is the main loop for WIZARDS typing QUIT will exit the repl"
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read()
"location 2560"
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")" ))))
    (flet ((quote-it (x)
		     (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;location 2595
(defparameter *allowed-commands* '(look move take inventory new-game))

(defun game-eval (sexp)
"location 2595"
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))

(defun tweak-text (lst caps lit)
"location 2647 - GAME-PRINT helper"
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
"location 2647"
  (princ (coerce (tweak-text (coerce (string-trim "()"
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))

(defun have (object)
"location 9106"
  (member object (inventory)))

(defmacro game-action (command subj obj place &body body)
"location 9173"
  `(progn (defun ,command (subject object)
	    (if (and (eq *location* ',place)
		     (eq subject ',subj)
		     (eq object ',obj)
		     (have ',subj))
		,@body
	      '(i cant ,command like that.)))
	  (pushnew ',command *allowed-commands*)))

;location 9106
(defparameter *chain-welded* nil)
(game-action weld chain bucket attic
	     (if (and (have 'bucket) (not *chain-welded*))
		 (progn (setf *chain-welded* 't)
			'(the chain is now securely welded to the bucket.))
	       '(you do not have a bucket.)))

(setf *bucket-filled* nil)

(game-action dunk bucket well garden
	     (if *chain-welded*
		 (progn (setf *bucket-filled* 't)
			'(the bucket is now full of water))
	       '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
	     (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
		   ((have 'frog) '(the wizard awakens and sees that you stole his frog.
				       he is so upset he banishes you to the netherworlds- you lose? the end.))
		   (t '(the wizard awakens from his slumber and greets you warmly.
			    he hands you the magic low-carb donut- you win! the end.))))

;;ugraph->png is found in graph.lsp and is responsible for writing GRAFVIZ readable .dot files
(defun draw-map (map node edge) 
  (ugraph->png map node edge))


(draw-map  *dot-file* *wizard-nodes* *wizard-edges*)
;(draw-map *dot-file-known* *visited-nodes* *wizard-edges*)
