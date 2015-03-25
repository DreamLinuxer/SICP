(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream-head (expand 1 7 10) 10) ; 1/7 = 0.14285...
(stream-head (expand 3 8 10) 10) ; 3/8 = 0.375
