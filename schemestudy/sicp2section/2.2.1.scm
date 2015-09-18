;;2.17

(define (last-pair items)
  (if (null?  items)
     	'()
	(last-pair (cdr items))))

(last-pair (list 23 72 149 34 ))

;;2.18 . modify

(define (reverse items)
  (define (minus-last items) 
    (if (null? (cdr  items)) 
	'()
	(cons (car items) (minus-last (cdr items)))))
  (if (null?  items)
      '()
      (cons  (car (last-pair items)) (reverse (minus-last items)))))

(reverse (list 1 4 9 16 25))

;;2.18简洁版正向思维,效率比较高
;;; 18-reverse.scm

(define (reverse lst)
    (iter lst '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))



;;;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 30 10 5 2 1 0.5))

;; (define (cc amount coin-values) 
;;   (define first-denomination 
;;     (car coin-values))
;;   (cond ((= amount 0)  1) 
;; 	((or (< amount 0) (no-more? coin-values)) 0)
;; 	(else (+ (cc amount (except-first-denomination coin-values))
;; 		 (cc (- amount first-denomination) coin-values)))))


(define (cc amount coin-values) 
  (cond ((= amount 0)  1) 
	((or (< amount 0) (no-more? coin-values)) 0)
	(else (+ (cc amount (except-first-denomination coin-values))
		 (cc (- amount (  first-denomination coin-values)) coin-values)))))


(define (first-denomination coin-values)
(car coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  	(cdr coin-values))

(cc 100 us-coins)
(cc 100 uk-coins)

;;;2.20

(define  ( even? n) (= (remainder  n 2) 0))
(define  (same-parity x . w)
  (define  (parity-iter  new-items remained-items)
    	   (if (null? remained-items)
	       new-items
	       (if ( even? (- x  (car remained-items)) )
		   (parity-iter (insert-tail new-items (car remained-items))  (cdr remained-items) )
		   (parity-iter new-items (cdr remained-items))
		   )
	       )) 
  (define (insert-tail items value)
   (if (null? items) 
      (cons value '())
      (cons (car items) (insert-tail (cdr items) value) )))
  (cons x (parity-iter  '()  w )))  

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;;; 对表的映射

(define nil '())
(define (scale-list items factor ) 
  	(if (null? items) 
	    nil
	    (cons (* (car items) factor)
		  (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5 ) 10)

(define (map proc items) 
  (if (null? items)  
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))


(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x))
     (list 1 2 3 4))

(define (scale-list items factor)
  	(map (lambda (x) (* x factor) ) items ))


;;; 2.21
(square-list (list 1 2 3 4))

(define (square-list items)
  (define square (lambda (x) (* x x)))
  (if (null? items) 
      nil
      (cons  (square (car items)) (square-list  (cdr items)))))



(define (square-list items)
  	(map square items))

;;; 2.22其实前几天已经研究过了表结构不同
(define (square-list items) 
  (define square (lambda (x) (* x x)))
  (define (iter things answer)
    	  (if (null? things)
	      	answer
		(iter (cdr things) 
		      ( cons   (square (car things))  answer))))
  (iter items nil))

 (square-list (list 1 2 3 4))



(define (square-list items) 
  (define square (lambda (x) (* x x)))
  (define (iter things answer)
    	  (if (null? things)
	      	answer
		(iter (cdr things) 
		      ( cons   answer  (square (car things))  ))))
  (iter items nil))

;;; 2.23
(define (for-each proc items) 
  (define (iter remained-items f)  
    	  (if (null? remained-items) 
	      (newline)
	     (iter (cdr remained-items) (proc (car remained-items)))))
	(iter items abs))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
