(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
	((while? exp) (eval (while->combination) env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (while? exp) (tagged-list? exp 'while))
(define (while-cond exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while->combination exp)
  (sequence->exp
   (list
    (list 'define (list 'iter)
	  (make-if (while-cond exp)
		   (sequence->exp (list
				   (while-body exp)
				   (list 'iter)))
		   true))
    (list 'iter))))

(while->combination
 (list 'while '(< x 10) (list '(set! x (+ x 1)) '(set! y x ))))
