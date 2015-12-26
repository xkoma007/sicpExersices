(define (force delayed-object)
	(delayed-object))


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
;(define (cons-stream a b) (cons a (delay b)))


(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-proc (lambda () b))))))

(define (memo-proc proc)
	(let ((already-run? false) (result false))
		 (lambda ()
			(if (not already-run?)
				(begin (set! result (proc))
                                       (set! already-run? true)
                                       result)
				result))))
			
(define (delay exp-value)
	    (memo-proc (lambda () exp-value)))
	

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

		

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s))
			   (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
			   
		
(define (display-stream s)
		(stream-for-each display-line s))
	
(define (display-line x)
		(newline)
		(display x))
		
		
;;test 3.50

(define (stream-map-extend proc . argstreams)
  (if (stream-null? (car argstreams))
       the-empty-stream
      (cons-stream 
       (apply proc (map stream-car argstreams))
       (apply stream-map-extend
              (cons proc (map stream-cdr argstreams))))))


		
