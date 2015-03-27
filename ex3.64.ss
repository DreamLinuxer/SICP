(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ((s1 (stream-car s))
	(s2 (stream-car (stream-cdr s))))
    (if (< (abs (- s1 s2)) tolerance)
	s2
	(stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1)
(sqrt 2 0.01)
(sqrt 2 0.001)

