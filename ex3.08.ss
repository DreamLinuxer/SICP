(define f
  (let ((v1 0)
	(even #t))
    (define (fun x)
      (let ((v2 v1))
	(set! v1 x)
	(set! even (not even))
	(if even (set! v1 0) #f)
	v2))
    fun))

(let ((f1 (f 0)))
  (let ((f2 (f 1)))
    (+ f1 f2)))

(let ((f1 (f 1)))
  (let ((f2 (f 0)))
    (+ f1 f2)))
