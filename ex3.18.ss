(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (let ((L '()))
    (define (iter x)
      (cond ((null? x) #f)
	    ((memq (car x) L) #t)
	    (else
	     (if (null? L)
		 (set! L (list (car x)))
		 (set-cdr! (last-pair L) (list (car x))))
	     (iter (cdr x)))))
    (iter x)))

(define z (make-cycle (list 'a 'b 'c)))
(cycle? z)
(define y (list 'a 'b 'c))
(cycle? y)
