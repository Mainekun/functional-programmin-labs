;;; Example graph 1 - 5 vertex 7 edges
(defun eg1 () '((1 2) (1 4) (2 3) (3 4) (4 5) (4 1) (5 1)))
(defun vg1 () '(1 2 3 4 5))

;;; modification of 'last' - returns last element (not in list!)
(defun llast (arr) 
	(declare (type list arr))
	(car (last arr))
)

;;; find incident to vertex edges
;;; v - vertex (num)
;;; es - list of edges (list of pair nums)
(defun find-incidents (v es)
	(cond
		((null es) '())
		((= (caar es) v) (cons (car es) (find-incidents v (cdr es))))
		(t (find-incidents v (cdr es)))
	)
)

;;; es - list of paths '(((1 2)(2 3)) ((1 2)(2 4)(4 5)) (...) ...)
;;; edge - edge to add
(defun add-edge (edge es)
	(cond
		((null es) '())
		(t (cons (append (car es) (list edge)) (add-edge edge (cdr es))))
	)
)

;;; vinit - initial vertex [vertex]
;;; vcur - current vertex [vertex]
;;; vs - available vertices [list]
;;; vall - all vertices [list]
;;; es - incident edge [edge]
;;; eall - available edges [list]
(defun find-cycles-next-vertex (vinit vcur vs vall es eall)
	(cond
		((null vs) nil)
		((null eall) nil)
		((null es) nil)
		((= vcur vinit) (list es))
		((member es eall) 
			(add-edge	
				es
				(find-cycles-main-recursion
					vinit
					vcur
					(remove vcur vs)
					vall
					nil 
					(remove es eall)
				)
			)
		)
		(t nil)
	)
)

;;; accepts: available vertices, available edges
;;; returns: list of cycles - '( ((1 4)(4 1)) ((1 4)(4 5)(5 1)) (...) ... )
;;; vinit - initial vertex
;;; vcur - current vertex
;;; vs - available vertices
;;; vall - all vertices
;;; es - incident edges
;;; eall - available edges
(defun find-cycles-edge-recursion (vinit vcur vs vall es eall)
	(cond
		((null es) nil)
		(t (cons
			(find-cycles-edge-recursion vinit vcur vs vall (cdr es) eall)
			(find-cycles-next-vertex vinit (llast (car es)) vs vall (car es) eall)
		))
	)
)

(defun find-cycles-main-recursion (vinit vcur vs vall es eall)
	(find-cycles-edge-recursion 
		vinit ;;; initial vertex 
		vcur ;;; current vertex
		(remove vcur vs) ;;; available vertex - we won't step in vertex only once
		vall ;;; all vertices
		(find-incidents vcur eall) ;;; incident edges
		eall ;;; available edges - we use edge only once
	)
)

(defun pr (a)
	(cond
		((null a) nil)
		(t (print (car a)) (write-line "") (pr (cdr a)))
	)
)

(defun entra ()
(trace find-cycles-main-recursion)
(trace find-cycles-edge-recursion)
(trace find-cycles-next-vertex)
)

(defun untra ()
(untrace find-cycles-main-recursion)
(untrace find-cycles-edge-recursion)
(untrace find-cycles-next-vertex)
)

(defun testeg ()
	(find-cycles-main-recursion 1 1 (vg1) (vg1) (eg1) (eg1))
)

(defun tests ()
	(princ "find-incidents 1 '((1 2)(1 4)(2 3)(3 4)(4 1)(4 5)(5 1))")
	(write-line "")
	(princ (find-incidents 1 '((1 2)(1 4)(2 3)(3 4)(4 1)(4 5)(5 1))))
	(write-line "")
	(princ "add-edge '(3 1) '(((1 2)(2 3)) (1 4)(4 3))")
	(write-line "")
	(print (add-edge '(3 1) '(((1 2)(2 3)) ((1 4)(4 3)))))
	(write-line "")
)
