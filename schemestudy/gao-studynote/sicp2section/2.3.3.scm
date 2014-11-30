;;; 集合作为未排序的表
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

;;;test  2.59
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

;;; 2.60

;;; 集合作为排序的表

(define (element-of-set? x set)
  (cond ((null? set) false)
	     ((= x (car set)) true)
	     ((< x (car set)) false)
	     (else (element-of-set? x (cdr set)))))   ;

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let  ((x1 (car set1))  (x2 (car set2)))
	      (cond  ((= x1 x2)   
		            (cons x1 (intersection-set (cdr set1)  (cdr set2))))
		          ((< x1 x2)
			   (intersection-set (cdr set1)  set2))
			  (else
			   (intersection-set  set1  (cdr set2)))))))  ;O(N)

;;; 2.61
;;; 2.62

;;; 集合作为二叉树
(define (entry tree) (car tree))
(define (left-branch tree)  (cadr tree))
(define (right-branch tree)  (caddr tree))
(define (make-tree entry left right)
(list entry left  right))


(define (element-of-set? x set)
  (cond  ((null? set)  false)
	      ((= x (entry set)) true)
	      ((< x (entry set)))
	      ((< x (entry set)) 
	       (element-of-set? x (left-branch set)))
	      ((> x (entry set)) 
	       (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
         (cond ((null? set)   (make-tree '() '()))
	            ((= x (entry set)) set)
		    ((< x (entry set)) 
		      (make-tree (entry set)
				           (adjoin-set x (left-branch set))
					   (right-branch set)))
		    ((> x (entry set))
		      (make-tree (entry set)
				           (left-branch set)
					   (adjoin-set x (right-branch set))))))

;;; 集合与信息检索

(define  (lookup given-key set-of-records)
    (cond ((null? set-of-records) false)
	       ((equal? given-key    (key (car set-of-records)))
		(car set-of-records))
	        (else (lookup given-key (cdr set-of-records)))))          

;;; 2.66


;;; 2.3.4 Huffman 编码树


;;; Huffman 树的表示
















