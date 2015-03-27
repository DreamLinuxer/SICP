(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 7)

(define (rand input-stream init)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((m (stream-car input-stream)))
	(cond ((eq? m 'generate)
	       (cons-stream init (rand (stream-cdr input-stream) (rand-update init))))
	      ((and (pair? m) (eq? (car m) 'reset))
	       (cons-stream (cdr m) (rand (stream-cdr input-stream) (rand-update (cdr m)))))
	      (else the-empty-stream)))))

(define test-stream
  (cons-stream 'generate
	       (cons-stream 'generate
			    (cons-stream (cons 'reset 10)
					 (cons-stream 'generate
						      (cons-stream 'generate
								   the-empty-stream))))))

(stream->list (rand test-stream random-init))
