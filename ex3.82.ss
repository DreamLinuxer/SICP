(define (random-number)
  (define random-numbers
    (cons-stream (random 1.0) (stream-map (lambda (x) (random 1.0)) random-numbers)))
  random-numbers)

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high r)
  (let ((range (- high low)))
    (+ low (* range r))))

(define (estimate-integral P x1 x2 y1 y2)
  (let ((r1 (stream-map (lambda (x) (random-in-range x1 x2 x)) (random-number)))
	(r2 (stream-map (lambda (x) (random-in-range y1 y2 x)) (random-number))))
    (stream-map
     (lambda (x) (* (- x2 x1) (- y2 y1) x))
     (monte-carlo (stream-map P r1 r2) 0 0))))

(define pi-stream
  (estimate-integral (lambda (x y) (< (+ (* x x) (* y y)) 1.0))
		     -1.0 1.0 -1.0 1.0))

(stream-ref pi-stream 100000)

