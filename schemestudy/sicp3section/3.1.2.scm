

(define rand
     (let ((x 7))
	  (lambda  ()
		 (set! x (rand-update x))
		 x)))
(rand)


(define (estimate-pi trials)
     (sqrt ( / 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
     (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
         (cond ((= trials-remaining 0)
		     (/ trials-passed trials))
	           ((experiment)
		    (iter (- trials-remaining 1) (+ trials-passed 1)))
		   (else (- trials-remaining 1) trials-passed)))
    (iter trials 0))


;;; test 3.5 


;;; test 3.6


(define a
(lambda (x) 
  (+ x 2) 4))

(a 5)
