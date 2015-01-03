;;; 针对上一节基于类型导向的程序设计方法，为了能够实现可加性，并管理更复杂的程序，
;;; 我们需要采用 新的程序设计方法：数据导向的程序设计。



(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))



;;; Ben
(define  (install-rectangular-package)
              ;; internal procedures
              (define (real-part z) (car z))
	      (define (imag-part z) (cdr z))
	      (define (make-from-real-imag x y) (cons x y))
	      (define (magnitude z)
		(sqrt   (+ (square (real-part z)) 
			   (square (imag-part z)))))
	      (define (angle z)
		            (atan (imag-part z) (real-part z)))
	      (define (make-from-mag-ang r a)
		           (cons (* r (cos a)) (* r (sin a))))
	      ;; interface to the rest of the system
	      (define (tag x) (attach-tag 'rectangular x))
	      (put 'real-part '(rectangular) real-part)
	      (put 'imag-part '(rectangular) imag-part)
	      (put 'magnitude '(rectangular) magnitude)
	      (put 'angle '(rectangular) angle)
	      (put 'make-from-real-imag 'rectangular
		  (lambda (x y) (tag (make-from-real-imag x y))))
	      (put 'make-from-mag-ang 'rectangular
		   (lambda (r a) (tag (make-from-mag-ang r a))))
	      'done)

(install-rectangular-package)
(get 'imag-part '(rectangular))
(get 'imag-part (list 'rectangular 'rectangular ))
(get 'imag-part '(polar))
;; (install-polar-package)


;;; Alyssa
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
        (* (magnitude z) (cos (angle z))))
  (define (imag-part z) 
       (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
       (cons  (sqrt (+ (square x) (square y)))
	           (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;;; General Apply
(define (type-tag datum)
  (if    (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if  (pair? datum)
       (cdr datum)
       (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
        (if proc
	    (apply proc (map contents args))
	    (error 
	     " No method for these types -- APPLY-GENERIC"
	     (list op type-tags))))))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag  'rectangular) x y))
(define (make-from-mag-ang r a)
 ((get 'make-from-mag-ang 'polar) x y))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; test 2.73

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;; 	         ((variable? exp) (if (same-variable? exp var) 1 0))
;; 		 ((sum? exp)
;; 		  (make-sum (deriv (addend exp ) var)
;; 			               (deriv (augend exp) var)))
;; 		 ((product? exp)
;; 		  (make-sum 
;; 		      (make-product (multiplier exp)
;; 				                 (deriv (multiplicand exp) var))
;; 		      (make-product (deriv (multiplier exp) var)
;; 				                 (multiplicand exp))))
;; 		 ;;other rules
;; 		 (else (error "unknown expression type -- DERIV" exp))))


;; a.  number? same-varibale? 不能对 数据进行简化，只能判断类型，它们不能够完整的描述一个表达式，所以这里我们不能够进行数据导向分派。

;;b.
         (define (variable? x)  (symbol? x))
                    (define (same-variable? v1 v2)
		                  (and (variable? v1) (variable? v2) (eq? v1 v2)))
                   (define (=number? exp num)
		                 (and (number? exp) (= exp num)))

             (define (deriv exp var)
	                  (cond  ((number? exp) 0)
			      	      ((variable? exp) (if (same-variable? exp var) 1 0))
				      (else  ((get 'deriv (operator exp))  (operands exp)
			                                                             var))))
             (define (operator exp) (car exp))
             (define (operands exp) (cdr  exp))


;;; make-product  simplified
(define (make-product m1 m2)
             (cond ((or (=number? m1 0) (=number? m2 0)) 0)
		        ((=number? m1 1) m2)
			((=number? m2 1) m1)
			((and (number? m1) (number? m2)) (* m1 m2))
			(else (list '* m1 m2))))

(define (make-sum a1 a2) 
              (cond ((=number? a1 0) a2)
		         ((=number? a2 0) a1)
			  ((and (number? a1) (number? a2)) (+ a1 a2))
			  (else (list '+ a1 a2))))

(define (install-add-func)
  (define (add-deriv exp var)
                           (make-sum (deriv (car exp) var)
				                (deriv (cadr exp) var)))
   (put  'deriv  '+ add-deriv)
 'done)

   (define (install-product-func)
  (define (product-deriv exp var) 
                           (make-sum 
			    (make-product  (cadr exp)
			                	        (deriv (car exp) var))
			    (make-product (car exp)
					               (deriv (cadr exp) var))))
                            (put  'deriv  '*   product-deriv)
	      'done)

      (install-add-func)
             (install-product-func)


(deriv '(* (* x y) (+ x 3))  'x)
(deriv '(+ x 3)  'x)
;;; c.

(define (install-exponentiation-func)
              (define (make-exponentiation  u n )
		(if (> n 1) 
		    (list  '** u n)
		    u))
	      (define (base x) (car x))
	      (define (exponent x) (cadr x))

	      (define (exponentiation-func exp var)
   	                   (make-product (exponent exp)
					              (make-product 
						       (make-exponentiation 
							(base exp) (- (exponent exp) 1))
						       (deriv (base exp) var))))
	      (put 'deriv '** exponentiation-func)
             'done)
(install-exponentiation-func)
(deriv '(+ (** x 5)  (* y x)) 'x)
;;; d.

exc

;;; test 2.74


;;; message dispatch

(define (make-from-real-imag x y)
              (define (dispatch op)
		 (cond  ((eq? op 'real-part) x)
			     ((eq? ip 'imag-part) y)
			     ((eq? op 'magnitude)
			      (sqrt (+ (square x) (square y))))
			     ((eq? op 'angle) (atan y x))
			     (else (error "Unknown ip -- MAKE-FROM-REAL-IMAG" op))))
	      dispatch)

(define (apply-gereric op arg)  (arg op))
