(defun fact (n) ; for check
	(cond
		((< n 1) 1)
		(t (* (fact (- n 1)) n))
	)
)

(defun C (n k) ; for check
	(/
	  	(fact n)
		(* (fact k) (fact (- n k)))
	)
)

(defun Ctotal (n i) ; Total sum of C(n,k) from n-size list. for check
  ; Ctotal = sum_{i=1}^{n}C_n^i
	(cond 
	  	((> i n) 0)
		(t (+ (Ctotal n (+ i 1)) (C n i)))
	)
)

(defun spec-map (arr n) ; appends n to the head of each list in array. arr - array of lists, n - element
	(cond
		((null arr) '())
		(t (cons (cons n (car arr)) (spec-map (cdr arr) n)))
	)
)

(defun combs-w/-nil (arr) ; (NIL (...) (...) ...)
	(cond
		((null arr) '(()))
		(t (append (combs-w/-nil (cdr arr)) 
			   (spec-map (combs-w/-nil (cdr arr)) (car arr)))
	 	)
	)
)

(defun combs-w/o-nil (arr) ; ((...) (...)...)
  	(cdr (combs-w/-nil arr))
)

(defun gen-row (n &optional (i 0))
	(cond
		((= i n) '())
		(t (cons i (gen-row n (+ i 1))))
	)
)

(defun gapcfle (n) ; generate all possible combinations for list elements
	(combs-w/o-nil (gen-row n))
)

(defun ufa (narr arr) ; unpack from array. narr - indicies array, array - source array
	(cond
		((null narr) '())
		(t (cons (nth (car narr) arr) (ufa (cdr narr) arr)))
	)
)

(defun ared (arr) ; reduce list with addition
	(cond
		((null arr) 0)
		(t (+ (car arr) (ared (cdr arr))))
	)
)

(defun bag-check (itemarr sizearr costarr B K) ; check combination for meeting condition
	(cond
		((and (<= (ared (ufa itemarr sizearr)) B) (>= (ared (ufa itemarr costarr)) K)) t)
		(t nil)
	)
)

(defun check-combs (combs sizearr costarr B K)
	(cond 
		((null combs) nil) ; base case
		((bag-check (car combs) sizearr costarr B K) (car combs)) ; meet condition
		(t (check-combs (cdr combs) sizearr costarr B K)) ; recursion
	)
)

(defun bag-fun (sizearr costarr B K)
	(cond
		((not (= (length sizearr) (length costarr))) nil) ; idiot protection
		(t (check-combs (gapcfle (length sizearr)) sizearr costarr B K))
	)
)
