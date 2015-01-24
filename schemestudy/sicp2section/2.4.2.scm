;;; 类型标志符号:

;; 构造函数 和 选择函数能够成功实现抽象隔离的作用。构造函数负责屏蔽事务的具体实 现细节，而选择函数则可以从复杂的事物中取得自己想要的结果。
   

(define  (attach-tag type-tag contents)
 (cons type-tag contents))

(define (type-tag datum)
  (if    (pair? datum)
	  (car datum)
	  (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if  (pair? datum)
       (cdr datum)
       (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z) 
  (eq? (type-tag z) 'polar))

;;; Ben
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(define (make-from-real-imag-rectangular  x y)
 (attach-tag 'rectangular  (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular  (cons (* r (cos a)) (* r (sin a)))))

;;; Alyssa polar
(define (real-part-polar z)
  (*   (magnitude-polar z) (cos (angle-polar z))))

          (define (imag-part-polar z)
	    (* (maginitude-polar z)  (sin (angle-polar z)))))

(define (magintude-polar z) (car z))

(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)  
  (cons  (sqrt (+ square x) (square y))))

(define (make-from-mag-ang-polar r a)
   (attach-tag 'polar (cons r a)) )


;;;  general
(define (real-part z)
  (cond ((rectangular? z)
	            (real-part-rectangular (contents z)))
	     ((polar? z)
	       (real-prat-polar (contents z)))
	    (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
	       (imag-part-rectangular  (contents z)))
	     ((polar? z)
	      (imag-part-polar (contents z)))
	     (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
              (cond ((rectangular? z) 
		          (magnitude-rectangular (contents z)))
		        ((polar? z)
			 (magnitude-polar (contents z)))
			(else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z) 
  (cond  ((rectangular? z)
	     (magnitude-rectangular (contents z))) 
	     ((polar? z)
	      (magnitude-polar (contents z)))  
	     (else (error "Unknown type -- ANGLE" z))))

;;; add-complex
(define  (add-complex z1 z2)
              (make-from-real-imag (+ (real-part z1) (real-part z2))
				                      (+ (imag-part z1) (imag-part z2))))

(define (make-from-real-img  x y)
             (make-from-real-imag-rectangular  x y))

(define (make-from-mag-ang r a)
             (make-from-mag-ang-polar r a)) 






 










