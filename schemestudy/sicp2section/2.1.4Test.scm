(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))
 
(define (mul-interval x y) 
        (let ((p1 (* (lower-bound x) (lower-bound y)))
	      (p2 (* (lower-bound x) (upper-bound y)))
	      (p3 (* (upper-bound x) (lower-bound y)))
	      (p4 (* (upper-bound x) (upper-bound y))))
	  (make-interval (min p1 p2 p3 p4)
			 (max p1 p2 p3 p4))))




(define (div-interval x y)
  (define (contain-zero value)
    (cond ( (and (< (lower-bound value) 0) (> (upper-bound value) 0)) 0)
		(else 1)))
  (if (= 0  ( contain-zero  y) )
      (display "不能进行处除法运算；") 
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0  ( lower-bound y))))))



(define (make-interval a b) (cons a b))

(define (upper-bound a ) 
  	(cdr a))


(define	(lower-bound b) 
  	(car b))


(define (sub-interval R1 R2) )

(define (make-center-percent  c p)
  (let ( (val (* c p)))
      (make-interval (- c val) (+ c val) )))

(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i) (/ (-  (upper-bound i) (lower-bound i) ) 2) )
(define (percent x)  
  (let ( (temp (center x)))
    (/ (- (upper-bound  x)  temp) temp)))



(define (par1 r1 r2) 
(div-interval (mul-interval r1 r2)
	      (add-interval r1 r2)))

(define (par2 r1 r2)
  	(let ((one (make-interval 1 1)))
(div-interval one (add-interval (div-interval one r1) (div-interval one r2))
 )))


(define A (make-interval 1.4  1.45))
(define B (make-interval 4.5  4.54))
(ma)
(make-center-percent)
					        
(div-interval A B)
(div-interval A A)
(center B)  
(percent B)
(make-center-percent (center B) (percent B))
(par1 A B)
(par2 A B)

;(define RAV (add-interval R1 R2))


