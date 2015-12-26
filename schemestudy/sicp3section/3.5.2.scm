;;������

(load "3.5.1.scm")

(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1))))

 (define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
	  (stream-filter (lambda (x) (not (divisible? x 7)))
	  integers))
	
(display (stream-ref no-sevens 7))


(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(display "  ")
(display (stream-ref primes 100))

;; 隐式的定义流

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map-extend + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(stream-car (stream-cdr (stream-cdr integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))



                
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(display "  ")
(display (stream-car (stream-cdr (stream-cdr double))))
(display "  ")
(display (stream-car double))
                       
                       
                       
                       
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))



(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


(display (prime? 8))
(display "  ")
(display (prime? 7))


;test 3.55

(define (partial-sums s1)
 (let ((curval 0))
   (define (data-sums s2)
     (begin (set! curval (+ curval (stream-car s2)))
            (cons-stream
             curval
             (data-sums (stream-cdr s2)))))
   (data-sums s1)))




;;(stream-car (partial-sums integers)
