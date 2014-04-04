;a
(define (make-generic-file division file)
  (cons division file))
(define (get-record employee generic-file)
  ((get 'get-record (car generic-file)) employee (cdr generic-file)))

;b
(define (make-generic-record division record)
  (cons division record))
(define (get-salary employee generic-record)
  ((get 'get-salary (car generic-record)) employee (cdr generic-record)))

;c
(define (in-this-file? employee generic-file)
  ((get 'in-this-file? (car generic-file)) employee (cdr generic-file)))
(define (find-employee-record employee generic-files)
  (if (null? generic-files)
      (error "employee not found")
      (if (in-this-file? employee (car generic-files))
	  (get-record employee (car generic-files))
	  (find-employee-record employee (cdr generic-files)))))

