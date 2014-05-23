(define (make-table)
  (define (entry tree) (car tree))
  (define (entry-key tree) (car (entry tree)))
  (define (entry-value tree) (cdr (entry tree)))
  
  (define (left-branch tree) (cadr tree))

  (define (right-branch tree) (caddr tree))

  (define (make-tree entry left right)
    (list entry left right))

  (define (assoc x tree)
    (cond ((null? tree) false)
	  ((= x (entry-key tree)) (entry tree))
	  ((< x (entry-key tree))
	   (assoc x (left-branch tree)))
	  ((> x (entry-key tree))
	   (assoc x (right-branch tree)))))

  (define (insert-tree x tree)
    (cond ((null? tree) (make-tree x '() '()))
	  ((< (car x) (entry-key tree))
	   (make-tree (entry tree)
		      (insert-tree x (left-branch tree))
		      (right-branch tree)))
	  ((> (car x) (entry-key tree))
	   (make-tree (entry tree)
		      (left-branch tree)
		      (insert-tree x (right-branch tree))))))

  (let ((table '()))
    (define (lookup key)
      (let ((record (assoc key table)))
	(if record
	    (cdr record)
	    false)))

    (define (insert! key value)
      (let ((record (assoc key table)))
	(if record
	    (set-cdr! record value)
	    (set! table (insert-tree (cons key value) table))))
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

(insert! 1 'a table)
(insert! 2 'b table)
(lookup 1 table)
(lookup 2 table)
(lookup 3 table)
(insert! 1 'c table)
(lookup 1 table)
