;; change lambda to : ((lambda parameter) (body))
(define (lambda? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'lambda)))

(define (lambda-parameters exp) (cadar exp))
(define (lambda-body exp) (cdr exp))

(define (make-lambda parameters body)
  (cons ('lambda parameters) body))
