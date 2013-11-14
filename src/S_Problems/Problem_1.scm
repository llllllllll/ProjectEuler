;; NOT YET COMPLETED.

(define problem-1 (n s)
  (cond ((> n 1000) (print s))
	((mod n 3) (problem-1 (n + 1) (s + n)))
	((mod n 5) (problem-1 (n + 1) (s + n)))
	(else problem-1 (n + 1) (s + n))))
