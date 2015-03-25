(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series s) (div-streams s integers))

(stream-head (integrate-series ones) 10)

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(stream-head exp-series 10)

(define cosine-series (cons-stream 1
				   (stream-map (lambda (x) (- x))
					       (integrate-series sine-series))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(stream-head cosine-series 10)
(stream-head sine-series 10)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
					 (scale-stream (stream-cdr s2) (stream-car s1)))
			    (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define one (add-streams (mul-series sine-series sine-series)
			 (mul-series cosine-series cosine-series)))

(stream-head one 10)
