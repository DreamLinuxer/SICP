(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define c3 (cons (cons '1 '2) (cons '3 '4)))
c3
(count-pairs c3)

(define c41 (cons '1 '2))
c41
(define c42 (cons '3 c41))
c42
(define c4 (cons c41 c42))
c4
(count-pairs c4)

(define c71 (cons '1 '2))
c71
(define c72 (cons c71 c71))
c72
(define c7 (cons c72 c72))
c7
(count-pairs c7)

(define cinf (cons '1 (cons '2 (cons '3 '4))))
(set-cdr! (cddr cinf) cinf)
(count-pairs cinf)

