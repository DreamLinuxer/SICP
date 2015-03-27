(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.000004)
(sqrt 10000000000000)

(define (my-good-enough? guess old-guess x)
  (< (abs (- guess old-guess)) 0.001))

(define (my-sqrt-iter guess old-guess x)
  (if (my-good-enough? guess old-guess x)
      guess
      (my-sqrt-iter (improve guess x)
		    guess
		    x)))

(define (my-sqrt x)
  (my-sqrt-iter 1.0 10.0 x))

(my-sqrt 0.000004)
(my-sqrt 10000000000000)
