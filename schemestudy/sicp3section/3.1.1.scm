;;; 赋值和局部状态

;;; 局部状态变量

(define balance 100)

;;; 语法 (set! <name> <new-value>)
(define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- banlance amount))
		      balance)
	  "Insufficient funds"))

;; (begin <exp1> <exp2> ... <expk>)
(define new-withdraw
     (let ((balance 100))
         (lambda (amount)
	   (if (>= balance amount)
	       (begin (set! balance (- balance amount))
		      balance)
	       "Insufficient funds"))))

(new-withdraw 20)
      (new-withdraw 10)

(define (make-withdraw balance)
       (lambda (amount)
	   (if (>= balance amount)
	       (begin (set! balance (- balance amount))
		      balance)
	       "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(w1 50)
(w1 40)

;;; zhanghu

;; (define (make-account balance)
;;      (define (withdraw amount)
;;           (if  (>= balance amount)
;; 	       (begin (set! balance (- balance amount))
;; 		      balance)
;; 	       "Insufficient funds"))
;;      (define (deposit amount)
;;          (begin   (set! balance (+ balance amount))
;; 		    balance))
;;     (define (dispatch m)
;;          (cond ((eq? m 'withdraw) withdraw)
;; 	            ((eq? m 'deposit) deposit)
;; 		    (else (error "Unknown request -- MAKE-ACCOUNT"
;; 				  m))))
;;     dispatch)

;; (define acc (make-account 100))
;; ((acc 'withdraw) 50)
;; ((acc 'withdraw) 60)
;; ((acc 'deposit) 40)

;;; 3.1
(define (make-accumulator base)
             (lambda (add-num)
	       (begin 
		     (set! base (+ base add-num))
		     base)))

(define A (make-accumulator 5))
(A 10)
(A 1124)

;;; 3.2

(define (make-monitored f )
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
	         ((eq? x 'reset-count) (set! count 0))
		 (else (begin (set! count (+ count 1)) 
			              (f x)))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)

;;; 3.3,3.4

(define (make-account   balance correct-password)
          (let ((count 0)) 
    (define (withdraw amount)
      (if  (>= balance amount)
	   (begin (set! balance (- balance amount))
		  balance)
	   "Insufficient funds"))  
    (define (deposit amount)
      (begin   (set! balance (+ balance amount))
	       balance))
    (define (call-the-cops useless)
      (error "the cops is coming\n"))
    (define (display-message-error useless-arg)
      (display "Incorrect password\n"))
    (define (handle-password-error count)
      (if (= count 7)
	  call-the-cops
	  display-message-error))
    (define (dispatch  password m)
      
      (if   (eq? password correct-password)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Unknown request -- MAKE-ACCOUNT"
			       m)))
	    (begin (set! count (+ 1 count))
		   (handle-password-error count))))
    dispatch))


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 50)
((acc 'some-other-password 'withdraw) 60)
((acc 'secret-password 'deposit) 40)








