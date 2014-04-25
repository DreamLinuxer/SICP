;;;-----------
;;;from section 3.3.3 for section 2.4.3
;;; to support operation/type table for data-directed dispatch

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;-----------

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
	((number? datum) datum)
	(else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (var-select x y)
    (if (symbol<? x y) y x))

  (define (expand-poly p)
    (expand-terms (variable p) (term-list p)))

  (define (canonical-poly p)
    (canonical-expansion (rearrange (expand-poly p))))

  (define (add-poly p1 p2)
    (canonical-poly
     (if (same-variable? (variable p1) (variable p2))
	 (make-poly (variable p1)
		    (add-terms (term-list p1)
			       (term-list p2)))
	 (make-poly (variable p1)
		    (add-terms (term-list p1)
			       (list (make-term 0 (tag p2))))))))

  (define (mul-poly p1 p2)
    (canonical-poly
     (if (same-variable? (variable p1) (variable p2))
	 (make-poly (variable p1)
		    (mul-terms (term-list p1)
			       (term-list p2)))
	 (make-poly (variable p1)
		    (mul-terms (term-list p1)
			       (list (make-term 0 (tag p2))))))))

  (define (zero-poly? p)
    (all-term-zero? (term-list p)))

  ;; Representing term lists

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (make-multi-term orders const) (cons orders const))
  (define (orders term) (car term))
  (define (make-order var order) (cons var order))
  (define (var-order o) (car o))
  (define (order-order o) (cdr o))
  (define (const term) (cdr term))

  (define (expand-terms var L)
    (if (empty-termlist? L)
	'()
	(append (expand-term var (first-term L))
		(expand-terms var (rest-terms L)))))

  (define (expand-term var t)
    (define (append-var-to-list var o L)
      (if (null? L) (list (cons var o))
	  (let ((current-var (car L)))
	    (cond ((symbol<? var (car current-var)) (cons (cons var o) L))
		  ((eq? var (car current-var)) (cons (cons var (+ o (cdr current-var))) (cdr L)))
		  (else (cons current-var (append-var-to-list var o (cdr L))))))))
    
    (let ((expansion (if (eq? (type-tag (coeff t)) 'polynomial)
			 (expand-poly (contents (coeff t)))
			 (list (make-multi-term '() (coeff t))))))
      (if (= 0 (order t))
	  expansion
	  (map (lambda (x) (cons (append-var-to-list var (order t) (orders x)) (const x))) expansion))))

  (define (rearrange expansion)
    (if (null? expansion) '() (insert-term (car expansion)
					   (rearrange (cdr expansion)))))
  
  (define (insert-term t L)
    (define (comp L1 L2)
      (cond ((and (null? L1) (null? L2)) 0)
	    ((null? L1) -1)
	    ((null? L2) 1)
	    (else (let ((t1 (car L1))
			(t2 (car L2)))
		    (if (eq? (var-order t1) (var-order t2))
			(let ((diff  (- (order-order t1) (order-order t2))))
			  (if (= diff 0) (comp (cdr L1) (cdr L2)) diff))
			(if (symbol<? (var-order t1) (var-order t2)) 1 -1))))))
    (if (null? L) (list t)
	(let ((result (comp (orders t) (orders (car L)))))
	  (cond ((> result 0) (cons t L))
		((< result 0) (cons (car L) (insert-term t (cdr L))))
		(else (make-multi-term (orders t) (add (const t) (const (car L)))))))))

  (define (canonical-expansion L)
    (if (null? L) 0
	(let ((first-orders (orders (car L))))
	  (if (null? first-orders) (const (car L))
	      (let ((p-var (var-order (car first-orders))))
		(tag (make-poly p-var
				(canonical-terms p-var L))))))))

  (define (canonical-terms p-var L)
    (define (get-principal-order t)
      (let ((O (orders t)))
	(if (or (null? O)
		(not (eq? p-var (var-order (car O)))))
	    (make-order p-var 0) (car O))))
    (define (same-principal-order? t1 t2)
      (let ((F1 (get-principal-order t1))
	    (F2 (get-principal-order t2)))
	(and (eq? (var-order F1) (var-order F2))
	     (= (order-order F1) (order-order F2)))))
    (define (split t L)
      (if (null? L) (cons '() '())
	  (if (same-principal-order? t (car L))
	      (let ((result (split t (cdr L)))
		    (first (car L))
		    (p-order (get-principal-order t)))
		(cons (cons (make-multi-term
			     (filter (lambda (x)
				       (not (and (eq? (var-order x) (var-order p-order))
						 (= (order-order x) (order-order p-order)))))
				     (orders first))
			     (const first))
			    (car result))
		      (cdr result)))
	      (cons '() L))))
    (define (grouping L)
      (if (null? L) '()
	  (let ((first (car L)))
	    (let ((groups (split first L)))
	      (cons (cons (order-order (get-principal-order first))
			  (car groups))
		    (grouping (cdr groups)))))))
    (define (trans-to-poly order-group)
	   (cons (car order-group)
		 (let ((group (cdr order-group)))
		   (let ((first (car group)))
		     (let ((ords (orders first)))
		       (if (and (= 1 (length group))
				(null? ords))
			   (list (const first))
			   (list (canonical-expansion group))))))))
    (map trans-to-poly (grouping L))
    )

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (all-term-zero? L)
    (if (empty-termlist? L) #t
	(and (=zero? (coeff (first-term L)))
	     (all-term-zero? (rest-terms L)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (add-poly p1 p2)))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (mul-poly p1 p2)))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)

  (put 'add '(polynomial scheme-number)
       (lambda (p n) (add-poly p (make-poly (variable p) (list (list 0 n))))))
  (put 'add '(scheme-number polynomial)
       (lambda (n p) (add-poly p (make-poly (variable p) (list (list 0 n))))))
  (put 'mul '(polynomial scheme-number)
       (lambda (p n) (mul-poly p (make-poly (variable p) (list (list 0 n))))))
  (put 'mul '(scheme-number polynomial)
       (lambda (n p) (mul-poly p (make-poly (variable p) (list (list 0 n))))))

  (put 'expand '(polynomial) expand-poly)
  (put 'rearrange '(polynomial) rearrange)
  (put 'canonical-expansion '(polynomial) canonical-expansion)
  (put 'canonical-poly '(polynomial) canonical-poly)
  
  'done)

(define (expand x) (apply-generic 'expand x))
(define (rearrange x) ((get 'rearrange '(polynomial)) x))
(define (canonical-expansion x) ((get 'canonical-expansion '(polynomial)) x))
(define (canonical x) (apply-generic 'canonical-poly x))

;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

(define a (make-polynomial 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))

a
;Value 3: (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))

(add a a)
;Value 4: (polynomial x (5 2) (4 4) (2 6) (1 -4) (0 -10))

(define b (make-polynomial 'x '((100 1) (2 2) (0 1))))

b
;Value 5: (polynomial x (100 1) (2 2) (0 1))

(mul b b)
;Value 6: (polynomial x (200 1) (102 4) (100 2) (4 4) (2 4) (0 1))

(define c (make-polynomial 'x (list '(2 1) (list 1 (make-polynomial 'y '((2 1) (1 2) (0 1)))) '(0 1))))

c

(add c a)

(mul c c)

(=zero? c)

(expand c)

(mul c c)

(expand (mul c c))

(define d (make-polynomial 'x (list '(2 1) (list 1 (make-polynomial 'y (list '(2 1) (list 1 (make-polynomial 'x '((1 2)))) '(0 1)))) '(0 1))))

d

(expand d)

(rearrange (expand d))

(rearrange (expand c))

(canonical-expansion (rearrange (expand d)))

(canonical d)

(add d a)

(mul a d)

(define e (make-polynomial 'x (list (list 1 (make-polynomial 'y (list '(2 1) (list 1 (make-polynomial 'x '((1 2))))))))))

e

(add e a)

(add e (make-polynomial 'x '((1 2))))

(define f  (make-polynomial 'y (list '(2 1) (list 1 (make-polynomial 'x '((1 2)))) '(0 1))))

f
(rearrange (expand f))
(canonical-expansion (rearrange (expand f)))
(add f a)

(mul f a)
