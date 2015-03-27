(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
		 (integral (scale-stream ones (/ 1.0 C)) v0 dt))))
