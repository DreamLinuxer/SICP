(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	    (cdr record)
	    false)))

    (define (assoc key records)
      (cond ((null? records) false)
	    ((same-key? key (caar records)) (car records))
	    (else (assoc key (cdr records)))))

    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
	(if record
	    (set-cdr! record value)
	    (set-cdr! table
		      (cons (cons key value) (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    (else (error "unknown message" m))))
    dispatch))

(define (lookup key table)
  ((table 'lookup) key))

(define (insert! key value table)
  ((table 'insert!) key value))

(define table
  (make-table (lambda (x y)
		(< (abs (- x y)) 0.01))))

(insert! 1.0 'a table)
(insert! 2.0 'b table)
(lookup 1.002 table)
(lookup 1.5 table)
(lookup 1.999 table)
(lookup 1.8 table)


