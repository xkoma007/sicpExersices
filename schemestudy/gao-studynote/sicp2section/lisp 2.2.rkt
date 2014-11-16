

(define (start-segment x)
  (car x))
(define (end-segment y)
  (cdr y))


(define (make-point m n)
  (cons m n))
(define (x-point x) (car x))
(define (y-point y) (cdr y))
(define (mid x y) (/ (+ x y) 2))

(define (make-segment  fpoint lpoint)
  (cons  fpoint lpoint))

(define (midpoint-segment l) 
        (make-point  (mid (x-point (start-segment l)) (x-point (end-segment l)))
                     (mid (y-point (start-segment l)) (y-point (end-segment l)))))

  (define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define f
(midpoint-segment (make-segment  (make-point 1.0 3.0) (make-point 4.0 3.0))))

(print-point f)



;愿望思维

(define (perimeter-rectangle r)
  (let ((length (length-of-rectangle r))
         (width (width-of-rectangle r)))
       (* 2 
          (+ length width))))

(define (area-rectangle r)
  (* (length-of-rectangle r)
     (width-of-rectangle r)))






  

