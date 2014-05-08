;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 7)			;**not in book**
(define rand
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
	     (set! x (rand-update x))
	     x)
	    ((eq? m 'reset)
	     (lambda (init)
	       (set! x init)
	       x))
	    (else (error "dispatch error" m))))
    dispatch))

(rand 'generate)
(rand 'generate)
((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
