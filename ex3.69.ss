(define integers (cons-stream 1 (stream-map (lambda (x) (+ x 1)) integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triple s1 s2 s3)
  (cons-stream
   (list (stream-car s1) (stream-car s2) (stream-car s3))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s1) x))
		(pairs (stream-cdr s2) (stream-cdr s3)))
    (triple (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))))

(define (Pythagorean? l) (= (+ (square (car l)) (square (cadr l))) (square (caddr l))))

(define Pythagorean-triples
  (stream-filter Pythagorean? (triple integers integers integers)))

(stream-head Pythagorean-triples 4)

