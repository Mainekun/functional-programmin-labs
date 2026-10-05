(defun y (x)
	(cond
		((<= x 4) nil) ; ограничение от корня и знаменателя
		(t (/ 1	(- 1 (cos (expt (- x 4) 1/4)))))
	)
)

(defun man-round (x n)
	(/ 
		(fround
			(*
				x
				(expt 10 n)
			)
		)
		(expt 10 n)
	)
)

(defun tabfunc (x0 xk h)
	(cond
		((> x0 xk) ())
		((not (y x0)) (format t "x: ~a y: NIL ~%" x0) (tabfunc (man-round (+ x0 h) (ceiling (abs (log h 10))) ) xk h))
		((and (> (y x0) -0.0001)(< (y x0) 0.0001)) (format t "x: ~a y: NIL ~%" x0)(tabfunc (man-round (+ x0 h) (ceiling (abs (log h 10))) ) xk h))
		(t 
			(format t "x: ~a y: ~,6F ~%" x0 (y x0))
			(tabfunc (man-round (+ x0 h) (ceiling (abs (log h 10))) ) xk h)
		)
	)
)

(tabfunc 3.5 4.5 0.1)
(write-line "")
(tabfunc -5 5 1)

