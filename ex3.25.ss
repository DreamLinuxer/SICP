(define (make-table)
  (let ((table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
	(if record
	    (cdr record)
	    false)))

    (define (assoc key records)
      (cond ((null? records) false)
	    ((equal? key (caar records)) (car records))
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

(define table (make-table))

(insert! '(a b c d) 1 table)
(insert! '(x y z) 2 table)
(insert! '(a b) 3 table)
(lookup '(a b c d) table)
(lookup '(a b c) table)
(lookup '(a b) table)
(lookup '(x y z) table)


