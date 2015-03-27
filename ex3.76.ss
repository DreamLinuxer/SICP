(define (smooth s)
  (let ((l (stream-cdr s)))
    (cons-stream (/ (+ (stream-car s) (stream-car l)) 2)
		 (smooth l))))

(define (make-zero-crossings input-stream smooth)
  (let ((s (smooth input-stream)))
    (stream-map sign-change-detector s (cons-stream 0 s))))
