

;;; 表格的表示
        (define  (lookup key table)
	        (let ((record (assoc key (cdr table))))
		   (if record
		       (cdr record)
		       false)))

      (define (assoc key records)
	    (cond ((null? records) false)
		      ((equal? key (car records))  (car records))
		      (else (assoc key (cdr records)))))
     

;;; 两维表格

;;; 创建局部表格
