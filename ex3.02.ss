(define (make-monitored f)
  (let ((count 0))
    (begin
      (define (dispatch m)
	(cond ((eq? m 'how-many-calls?) count)
	      (else (set! count (+ 1 count))
		    (f m))))
      dispatch)))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
