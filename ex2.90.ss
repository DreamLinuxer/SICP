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
(define (neg x) (apply-generic 'neg x))

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
  (put 'neg '(scheme-number)
       (lambda (x) (- x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

(define (install-dense-poly-package)
  (define (tag p) (attach-tag 'dense-poly p))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (adjoin-dense-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(if (= (order term) (length term-list))
	    (cons (coeff term) term-list)
	    (adjoin-dense-term term (cons 0 term-list)))))

  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (put 'adjoin-term '(term dense-poly) (lambda (t L) (tag (adjoin-dense-term t L))))
  (put 'the-empty-termlist '(dense-poly) (lambda (L) (tag '())))
  (put 'first-term '(dense-poly) first-term)
  (put 'rest-terms '(dense-poly) (lambda (L) (tag (rest-terms L))))
  (put 'empty-termlist? '(dense-poly) empty-termlist?)
  (put 'make-dense-poly 'dense-poly (lambda (L) (tag L)))
  'done)

(define (install-sparse-poly-package)
  (define (tag p) (attach-tag 'sparse-poly p))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (adjoin-sparse-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (put 'adjoin-term '(term sparse-poly) (lambda (t L) (tag (adjoin-sparse-term t L))))
  (put 'the-empty-termlist '(sparse-poly) (lambda (L) (tag '())))
  (put 'first-term '(sparse-poly) first-term)
  (put 'rest-terms '(sparse-poly) (lambda (L) (tag (rest-terms L))))
  (put 'empty-termlist? '(sparse-poly) empty-termlist?)
  (put 'make-sparse-poly 'sparse-poly (lambda (L) (tag L)))
  'done)

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-dense-poly variable term-list)
    (cons variable ((get 'make-dense-poly 'dense-poly) term-list)))
  (define (make-sparse-poly variable term-list)
    (cons variable ((get 'make-sparse-poly 'sparse-poly) term-list)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(cons (variable p1)
	      (add-terms (term-list p1)
			 (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(cons (variable p1)
	      (mul-terms (term-list p1)
			 (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))

  (define (zero-poly? p)
    (all-term-zero? (term-list p)))

  (define (neg-poly p)
    (cons (variable p) (neg-all-term (term-list p))))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

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
	(the-empty-termlist L1)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist L)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (all-term-zero? L)
    (if (empty-termlist? L) #t
	(and (=zero? (coeff (first-term L)))
	     (all-term-zero? (rest-terms L)))))

  (define (neg-all-term L)
    (if (empty-termlist? L) (the-empty-termlist L)
	(let ((t (first-term L)))
	  (adjoin-term (make-term (order t) (neg (coeff t)))
		       (neg-all-term (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make-dense-poly 'polynomial
       (lambda (var terms) (tag (make-dense-poly var terms))))
  (put 'make-sparse-poly 'polynomial
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))

  (put 'add '(polynomial scheme-number)
       (lambda (p n) (tag (add-poly p (make-dense-poly (variable p) (list n))))))
  (put 'add '(scheme-number polynomial)
       (lambda (n p) (tag (add-poly p (make-dense-poly (variable p) (list n))))))
  (put 'mul '(polynomial scheme-number)
       (lambda (p n) (tag (mul-poly p (make-dense-poly (variable p) (list n))))))
  (put 'mul '(scheme-number polynomial)
       (lambda (n p) (tag (mul-poly p (make-dense-poly (variable p) (list n))))))
  (put 'sub '(polynomial scheme-number)
       (lambda (p n) (tag (sub-poly p (make-dense-poly (variable p) (list n))))))
  (put 'sub '(scheme-number polynomial)
       (lambda (n p) (tag (sub-poly p (make-dense-poly (variable p) (list n))))))
  'done)

;; Constructor
(define (make-dense-polynomial var terms)
  ((get 'make-dense-poly 'polynomial) var terms))
(define (make-sparse-polynomial var terms)
  ((get 'make-sparse-poly 'polynomial) var terms))

(define (adjoin-term t L) (apply-generic 'adjoin-term (attach-tag 'term t) L))
(define (the-empty-termlist L) (apply-generic 'the-empty-termlist L))
(define (first-term L) (apply-generic 'first-term L))
(define (rest-terms L) (apply-generic 'rest-terms L))
(define (empty-termlist? L) (apply-generic 'empty-termlist? L))

(install-dense-poly-package)
(install-sparse-poly-package)
(install-polynomial-package)

(define a (make-dense-polynomial 'x '(1 2 0 3 -2 -5)))

a

(define b (make-sparse-polynomial 'x '((2 1) (1 2) (0 1))))

b

(neg a)

(neg b)

(add a a)

(sub a b)

(sub b a)

(mul a a)

(mul a b)

(mul b a)

(add a 1)

(sub b 1)

(mul b b)
