(define (smallest-divisor n)
       ( search-value1 2 n)
  )

(define  (search-value m n)
         (cond  (( > (square m) n) n)
                 (( = (remainder n m) 0) m)
                 ( else (search-value (+ m 1) n))))

(define  (search-value1 m n)
         (cond  (( > (square m) n) n)
                 (( = (remainder n m) 0) m)
                 ( else (if  ( = m 2) 
                             (search-value1 (+ m 1) n)
                             (search-value1 (next m) n)))))
                              

(define (square s) (* s s))
(define (next n)  (+ n 2))

  
(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n(runtime)))


(define (start-prime-test n start-time)
         (if (fast-prime? n 100)
            ( report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
        (display " ***")
        (display elapsed-time))



(define (prime? n) (= (smallest-divisor n) n))


(define (find-min n)
  (if(even? n) (find-min ( + n 1))
               ( if(prime? n)  n   (find-min (+ n 2)))))

(define (even? n)  (= (remainder n 2) 0))



(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1))) 
         (else false)))

(define (fermat-test n)
        (define (try-it a)
                (= (expmod a n n) a))
         (try-it (+ 1  (random (- n 1)))))

(define (expmod base exp m)
  (cond  ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m)) m))
          (else
            ( remainder (* base (expmod base (- exp 1) m)) m))))


( search-value1 2 19999)
(find-min 1)
(find-min 10)
(find-min 100)        
   (find-min 1000)
    (find-min 10000)
     (find-min 100000)
     (find-min 10000000)
     (find-min 10000000000000)
     
 (time-prime-test 100002)
 (time-prime-test 10000019)
 
(define (Carmichael n)
  ( if (fast-prime? n n) 1  0))

 