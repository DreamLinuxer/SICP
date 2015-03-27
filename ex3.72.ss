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

(define square-sum (lambda (l) (+ (square (car l))
				  (square (cadr l)))))

(define (find s)
  (let ((l1 (stream-cdr s)))
    (let ((l2 (stream-cdr l1)))
      (let ((car1 (stream-car s))
	    (car2 (stream-car l1))
	    (car3 (stream-car l2)))
	(let ((sum1 (square-sum car1))
	      (sum2 (square-sum car2))
	      (sum3 (square-sum car3)))
	  (if (= sum1 sum2 sum3)
	      (cons-stream (list sum1 car1 car2 car3) (find (stream-cdr l2)))
	      (find l1)))))))

(define square-sum-numbers (find (weighted-pairs integers integers square-sum)))

(stream-head square-sum-numbers 6)
