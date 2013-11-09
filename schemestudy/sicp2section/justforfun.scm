(define zero (lambda (f) 
	       (lambda (x) x)))
(define (add-1 n) 
  (lambda (f) (lambda (x) 
		(f ((n f) x)))))  	


(add-1 zero)

(add-1 
      (lambda (f)
	(lambda (x)
	  x)))

((lambda (n)                    ; add-1
     (lambda (f)
         (lambda (x)
             (f ((n f) x)))))
 (lambda (f)                    ; zero
     (lambda (x)
         x)))

(lambda (f)
    (lambda (x)
        (f (
            ((lambda (f)        ; zero
                 (lambda (x)
                     x))
             f)
            x))))


(lambda (f)
    (lambda (x)
        (f ((lambda (x) x)
            x))))

(lambda (f)
    (lambda (x)
        (f x)))
