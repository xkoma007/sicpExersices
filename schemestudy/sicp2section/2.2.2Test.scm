(define x (cons (list 1 2) (list 3 4)  ))
(count-leaves x)


(define (count-leaves x) 
(cond ((null? x) 0 )
      ( (not (pair? x))  1) 
      (else (+ (count-leaves (car x)) 
	       (count-leaves (cdr x)))) ))


;;; test2.24
(list 1 (list 2 (list 3 4)))
;;; test2.25
(cdr  (car  (cdr (cdr (1 3 (5 7) 9)))))	;1
(car  (car ((7))))			;2
;;; test2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(cons x y)
(list x y)
;;; test2.27
;;; from huangz


(define (reverse lst)
    (iter lst '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))



(define (tree-reverse lst)
    (define (iter remained-items result)
        (if (null? remained-items)
            result
            (iter (cdr remained-items)
                  (cons (if (pair? (car remained-items))
                            (tree-reverse (car remained-items))
                            (car remained-items))
                        result))))	
    (iter lst '()))

;;; 复合抽象看作一个表而不要自己拆开乱看

(define  (my-reverse items)   
  (define (iter  remained-items result)  
    	  (if (null? remained-items)
	      	result
	       (iter (cdr remained-items)  
		     (cons
		      (if (pair? ( car remained-items)) 
			    (my-reverse  (car remained-items) )
			    (car remained-items)) 
			result))))  
(iter items '()))
(define x (list (list 1 2) (list 3 4)))
(my-reverse x)
(tree-reverse x)

(define x (list (list 1 2) (list 3 4) (list 5 6)))
(define x (list 1 2))


;;; test2.28

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )))
(define (fringe x)
  (cond	((null? x) '()) 
	((not (pair? x))  (cons   x  '())) ;检查序列是否为序对
	(else  (append   (fringe (car x))
			 (fringe (cdr x))))))

(define x (list (list 1 2)  (list 3 4)))
(fringe   x)
(fringe (list x x))

;;; test2.29
(define (make-mobile left right)  
  (list left right) )

(define (make-branch length structure) 
  (list length structure))

;;; question a
(define  (left-branch mobile) 
  (car mobile))
(define (right-branch mobile) 
 (cdr mobile))

(define  (branch-length branch) 
(car branch) )
(define (branch-structure branch) 
(cdr branch))



;;; question b

(define (total-weight mobile)
  (cond  ((null? mobile) 0)
	 ((not (pair? mobile))  mobile )
	 (else  (+ (total-weight (car mobile)  
		    (total-weight (cdr mobile)))))) )
	 
   		
;;; c
(define balance (> 1 0))
(define non-balane (< 1 0))
 (define  (balance-mobile  mobile) 
  ( let ( 
	 (left-weight  (total-weight  (left-branch mobile)))
	 (left-length (branch-length (left-branch mobile)))
	 (right-weight (total-weight (right-branch mobile)))
	 (right-length (branch-length (right-branch mobile))))
    (cond  ( (null? mobile) balance) 
	   ( (not (pair? mobile))  non-balane)
	   ( else ( if ( and (balance-mobile  (car mobile))   
			     (balance-mobile  (cdr mobile)) 
			     (=  (* left-length left-weight)
				 (* right-length right-weight)))
		       balance
		       non-balane)))))


;;; d
没发现有什么区别啊。。    



;;; 对树的映射
(define   (scale-tree tree factor ) 
  (cond ((null? tree) nil)
	((not (pair? tree))  (* tree factor))
	(else (cons (scale-tree (car tree) factor )
		    (scale-tree (cdr tree) factor)))))  

(define nil '())
(scale-tre (list 1 (list 2 (list 3 4 ) 5) (list 6 7))  10) ;;;;; the first


(define (scale-tre tree factor) 
  (map  (lambda (sub-tree)
	  (if (pair? sub-tree)
	      (scale-tree sub-tree factor)
	    (* factor sub-tree) )) tree))

(define (map proc items)  
  (if (null? items) 
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))


;;; test 2.30

 (define nil
    '())
 (define (square x)
    (* x x))
(define (square-tree items)
  (cond  ((null? items) nil )
	 ( (not  (pair? items)) (square items))
	(else (cons (square-tree (car items))   
		    (square-tree (cdr items))))))


(define (square-trees trees)
  (map (lambda (sub-tree)
	 (cond ((not  (pair? sub-tree))  (square sub-tree)) 
	       (else (square-trees sub-tree))))
     trees))

;;; test 2.31
(define (square-tre tree)
  (tree-map square tree))
(define (tree-map proc  tree)
   (map (lambda (sub-tree)
	 (cond ((not  (pair? sub-tree))  (proc  sub-tree)) 
	       (else (tree-map proc sub-tree))))
     tree))

(square-tre (list 1 
	       (list 2 (list 3 4) 5) (list 6 7)))

;;; test 2.32
(define (subsets s)
  	(if (null? s)
	    (list nil)
	    (let ((rest (subsets (cdr s))))
	      (append rest (map (lambda (each-rest) 
				  (append (list (car s)) each-rest)) rest)))))
(subsets (list 1 2 3))






