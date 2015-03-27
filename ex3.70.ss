(define integers (cons-stream 1 (stream-map (lambda (x) (+ x 1)) integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
	   (let ((w1 (weight s1car))
		 (w2 (weight s2car)))
	     (cond ((< w1 w2)
		    (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
		   (else
		    (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define pair1 (weighted-pairs integers integers
			      (lambda (l) (+ (car l) (cadr l)))))

(stream-head pair1 20)

(define pair2 (weighted-pairs integers integers
			      (lambda (l) (+ (* 2 (car l))
					     (* 3 (cadr l))
					     (* 5 (car l) (cadr l))))))

(stream-head pair2 20)
