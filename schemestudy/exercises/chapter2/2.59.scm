
(define  (element-of-set? x set) 
             (cond   ((null? set) false)
		         ((equal? x (car set)) true)
			 (else  (element-of-set? x (cdr set)))))

 (define (adjoin-set x set)
   (if (element-of-set? x set)
       set
       (cons x set)))



(define  (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	     ((element-of-set? (car set1) set2)  
	       (cons (car set1)
		         (intersection-set (cdr set1) set2)))
	     (else (intersection-set (cdr set1) set2))))   ;因为 所需步数为 两个集合大小的乘积，所以说复杂度为 O(n2)


(define (union-set set1 set2)
  (cond  ((null? set1) set2)
	      ((null?  set2) set1)
	      ((element-of-set? (car set1) set2)
	        (union-set (cdr set1) set2))
	      (else (cons (car set1) (union-set (cdr set1) set2)))))

;;; test case
  (define  set1 (list 1 3 4 7 8 9 10))
 (define set2 (list 2 5 4 7 10 12))
(element-of-set?  3 set1)
(union-set set1 set2)


(intersection-set set1 set2)
