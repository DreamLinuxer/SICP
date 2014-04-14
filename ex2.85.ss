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
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (if (= (remainder x y) 0)
			      (/ x y)
			      (make-rational x y)))))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
		       (tag x)
		       (error "non-integer value" x))))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'project '(integer) (lambda (x) (make-integer x)))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
			  (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational) (lambda (x) (make-integer (round (/ (numer x) (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (/ x y)))
  (put 'make 'real
       (lambda (x) (if (real? x)
		       (tag x)
		       (error "non-real value" x))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real) (lambda (x)
			   (let ((rat (inexact->exact x)))
			     (make-rational (numerator rat) (denominator rat)))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
			  (= (imag-part x) (imag-part y)))))
  (put '=zero? '(complex)
       (lambda (x) (= 0 (magnitude x))))
  (put 'project '(complex) (lambda (x) (make-real (real-part x))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (raise x) (apply-generic 'raise x))

(define (install-raise-package)
  (put 'level 'integer 1)
  (put 'level 'rational 2)
  (put 'level 'real 3)
  (put 'level 'complex 4)
  'done)

(install-raise-package)

(define (project x) (apply-generic 'project x))

(define (drop x)
  (let ((dropped (project x)))
    (if (eq? (type-tag x) (type-tag dropped))
	x
	(let ((raised (raise dropped)))
	  (if (equ? x raised)
	      (drop dropped)
	      x)))))

(put 'add 'drop '())
(put 'sub 'drop '())
(put 'mul 'drop '())
(put 'div 'drop '())

(define (apply-generic op . args)
  (define (get-higher t1 t2)
    (let ((L1 (get 'level t1))
	  (L2 (get 'level t2)))
      (if (< L1 L2) t2 t1)))

  (define (find-highest type-tags)
    (let ((type (car type-tags)))
      (if (null? (cdr type-tags))
	  type
	  (get-higher type (find-highest (cdr type-tags))))))

  (define (raise-to target-type arg)
    (if (eq? target-type (type-tag arg))
	arg
	(raise-to target-type (raise arg))))

  (define (raise-list target-type args)
    (if (null? args) '()
	(cons (raise-to target-type (car args))
	      (raise-list target-type (cdr args)))))

  (define (apply-raise target-type)
    (let ((c-args (raise-list target-type args)))
      (let ((type-tags (map type-tag c-args)))
	(let ((proc (get op type-tags)))
	  (if proc
	      (apply proc (map contents c-args))
	      (error "No method for these types -- APPLY-GENERIC"
		     (list op type-tags)))))))
  (let ((result
	 (let ((type-tags (map type-tag args)))
	   (let ((proc (get op type-tags)))
	     (if proc
		 (apply proc (map contents args))
		 (apply-raise (find-highest type-tags)))))))
    (if (get op 'drop) (drop result) result)))

(project (make-integer 1))
(project (make-real 0.5))
(project (make-rational 9 3))
(project (make-complex-from-real-imag 1 2))

(drop (make-integer 1))
(drop (make-rational 9 3))
(drop (make-real 2.5))
(drop (make-real 1.))
(drop (make-complex-from-real-imag 1 0))

(add (make-real 0.5) (make-rational 1 2))
(add (make-complex-from-real-imag 1 0) (make-rational 1 2))
(add (make-complex-from-real-imag 1 1) (make-rational 1 2))
(mul (make-complex-from-real-imag 0.1 0) (make-real 10.0))
(real-part (mul (make-complex-from-real-imag 0.1 0.2) (make-real 10.0)))
(imag-part (mul (make-complex-from-real-imag 0.1 0.2) (make-real 10.0)))
