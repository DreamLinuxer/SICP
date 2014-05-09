(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((password-list (list password)))
    (define (dispatch p m)
      (if (memq p password-list)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		((eq? m 'make-joint)
		 (lambda (new-password)
		   (set! password-list (cons new-password password-list))
		   dispatch))
		(else (error "Unknown request -- MAKE-ACCOUNT"
			     m)))
	  (error "Incorrect password")))
    dispatch))

(define (make-joint acc password new-password)
  ((acc password 'make-joint) new-password))

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 40)
((paul-acc 'open-sesame 'deposit) 1000)

