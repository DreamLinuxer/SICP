(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series s) (div-streams s integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1
				   (stream-map (lambda (x) (- x))
					       (integrate-series sine-series))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
					 (scale-stream (stream-cdr s2) (stream-car s1)))
			    (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define one (add-streams (mul-series sine-series sine-series)
			 (mul-series cosine-series cosine-series)))

(define (invert-unit-series s)
  (cons-stream 1 (mul-series (invert-unit-series s)
			     (stream-map (lambda (x) (- x)) (stream-cdr s)))))

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "zero constant term of denominator")
      (let ((scale (stream-car s2)))
	(scale-stream (mul-series s1
				  (invert-unit-series (scale-stream s2 (/ 1 scale))))
		      scale))))

(define tangent-series (div-series sine-series cosine-series))

(stream-head tangent-series 10)
