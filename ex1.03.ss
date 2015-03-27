(define (sum-of-square x y) (+ (square x) (square y)))

(define (sum-of-square3 a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-square b c))
	((and (<= b a) (<= b c)) (sum-of-square a c))
	(else (sum-of-square a b))))

(sum-of-square3 1 2 4)
(sum-of-square3 2 1 4)
(sum-of-square3 2 4 1)
