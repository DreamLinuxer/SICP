(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (define (iter p q)
    (cond ((not (pair? q)) false)
	  ((not (pair? (cdr q))) false)
	  (else (let ((next-p (cdr p)))
		  (let ((next-q (cddr q)))
		    (if (eq? next-p next-q)
			true
			(iter next-p next-q)))))))
  (iter x x))

(define z (make-cycle (list 'a 'b 'c 'd)))
(cycle? z)
(define y (list 'a 'b 'c))
(cycle? y)
