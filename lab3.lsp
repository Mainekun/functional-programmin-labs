(defun sterling (n r)
  (cond
	((> r n) 0)
	((and (= r 0) (> n 0)) 0)
	((and (= r 0) (= n 0)) 1)
	(t (+
		(sterling (- n 1) (- r 1))
		(* r (sterling (- n 1) r))
	))
   )
)

(defun logsterling (n r)
	(format t "n=~a r=~a -> S=~a~%" n r (sterling n r))
)

(defun stdtest ()
	(logsterling 4 2)
	(logsterling 4 3)
)

(stdtest)

