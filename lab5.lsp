;;; Example graph 1 - 5 vertex 7 edges
(defun eg1 () '((1 2) (1 4) (2 3) (3 4) (4 5) (4 1) (5 1)))
(defun vg1 () '(1 2 3 4 5))
(defun eg2 () '((1 2) (2 3) (2 4) (3 4) (3 6) (4 1) (4 5) (4 6) (5 1) (5 6) (6 1)))
(defun eg2- () '((1 2) (2 3) (2 4) (3 4) (3 6) (4 1) (4 5) (4 6) (5 1) (5 6) (6 1) (6 2)))
(defun vg2 () '(1 2 3 4 5 6))

(defun pg2 () '(((1 2) (2 4) (4 6) (6 1)) ((1 2) (2 4) (4 5) (5 6) (6 1)) ((1 2) (2 4) (4 5) (5 1)) ((1 2) (2 4) (4 1)) ((1 2) (2 3) (3 6) (6 1)) ((1 2) (2 3) (3 4) (4 6) (6 1)) ((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)) ((1 2) (2 3) (3 4) (4 5) (5 1)) ((1 2) (2 3) (3 4) (4 1))))

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
		(t (cons (append (list edge) (car es)) (add-edge edge (cdr es))))
	)
)

;;; appends intermediate edge or returns last edge
(defun fcm3 (vi vc va ei ea)
	(cond
		((= (llast ei) vi) (list (list ei)))
		((null va) (list '()))
		((null ea) (list '()))
		((not (member (llast ei) va)) (list '()))
		((not (member ei ea)) (list '()))
		(t
			(add-edge
				ei
				(fcm1 vi (llast ei) va nil ea)
			)
		)
	)
)

;;; group in list 
(defun fcm2 (vi vc va ei ea)
	(cond
		((= (length ei) 1) (fcm3 vi vc va (car ei) (remove ei ea)))
		(t (append 
				 (fcm3 vi vc va (car ei) (remove ei ea)) 
				 (fcm2 vi vc va (cdr ei) ea)
		))
	)
)

;;; vi - v initial
;;; vc - v current
;;; va - v available
;;; ei - e incident
;;; ea - e available
;;; initiate new incident edges
(defun fcm1 (vi vc va ei ea)
	(fcm2 vi vc (remove vc va) (find-incidents vc ea) ea)
)

(defun fcm (va vs ei ea)
	(cond
		((null vs) '())
		(t (append (fcm1 (car vs) (car vs) va ei ea) (fcm va (cdr vs) ei ea) ))
	)
)

;;; delete non cycles
(defun dnc (ps)
	(cond
		((null ps) nil)
		((= (caar (car ps)) (cadar (last (car ps)))) 
		 (cons (car ps) (dnc (cdr ps))))
		(t (dnc (cdr ps)))
	)
)

(defun rot (p) 
	(append (cdr p) (list (car p)))
)

(defun is-rot (p pc i) 
	(cond
		((= i 0) nil)
		((equal p pc) t)
		((/= (length p) (length pc)) nil)
		(t (is-rot p (rot pc) (1- i)))
	)
)

(defun not-in (p acc) 
	(cond
		((null acc) t)
		((is-rot (car acc) p (length p)) nil)
		(t (not-in p (cdr acc)))
	)
)

;;; delete rotations
(defun ddr (ps &optional (acc nil))
	(cond
		((null ps) acc)
		((not-in (car ps) acc) (ddr (cdr ps) (cons (car ps) acc)))
		(t (ddr (cdr ps) acc))
	)
)

(defun itob (i)
	(cond 
		((= i 1) '(1))
		(t (cons (rem i 2) (itob (floor i 2))))
	)
)

(defun ritob (i)
	(reverse (itob i))
)

(defun add0n (b n)
	(cond
		((>= (length b) n) b)
		(t (cons 0 (add0n b (1- n))))
	)
)

(defun unpack-by-b (l b)
	(cond
		((null b) '())
		((/= (length l) (length b)) nil)
		((= (car b) 0) (unpack-by-b (cdr l) (cdr b)))
		(t (cons (car l) (unpack-by-b (cdr l) (cdr b))))
	)
)

(defun unpbn (l n)
	(unpack-by-b l (add0n (ritob n) (length l)))
)

(defun solution (vs es k)
	; (solrec (fcm vs vs es es) k)
)

(defun pr (a)
	(cond
		((null a) nil)
		(t (print (car a)) (write-line "") (pr (cdr a)))
	)
)

(defun entrad ()
(trace find-cycles-main-recursion)
(trace find-cycles-edge-recursion)
(trace find-cycles-next-vertex)
)

(defun etrace ()
(trace fcm1) (trace fcm2) (trace fcm3)
)

; (etrace)

(defun untrad ()
(untrace find-cycles-main-recursion)
(untrace find-cycles-edge-recursion)
(untrace find-cycles-next-vertex)
)

(defun utrace ()
(untrace fcm1) (untrace fcm2) (untrace fcm3)
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
