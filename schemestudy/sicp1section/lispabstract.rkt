(define (sqrt x)
   (define (good-enough? guess x previous)
(<  (/  ( abs (- guess previous) ) guess) 0.001))
     
     
     (define (improve guess x)
      (average guess (/ x guess) ))
    (define (average a b)
      (/ (+ a b) 2))
   (define (square guess) 
        (* guess guess))
    (define (sqrt-iter guess x previous)
       (if (good-enough? guess x previous)
           guess
           (sqrt-iter (improve guess x) x guess)));正确描述一个问题
    (sqrt-iter 1.0 x 0.0))
      


(define (sqrt3 x)
   (define  (sqr-iter3 guess )
     (if (good-enough? guess)
          (improve guess)
            (sqr-iter3 (improve guess))))
  
     (define (improve guess) 
           (/ (+  (/ x (square guess)) (* 2 guess)) 3))
    (define (square s) (* s s))
  
     (define (good-enough? guess)
        (<  (abs  ( - (improve guess) guess))  0.001))
             (sqr-iter3 1.0))



(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube a) (* a a a))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))



(define (identity x) x)
(define (sum-integers a b)
   (sum identity a inc b))
  
   (sum-integers 1 20)
   
   
(define (pi-sum a b)
   (define (pi-term x)
     (/ 1.0 (* x (+ x 2))))
     (define (pi-next x) (+ x 4))
     (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
  
  (integral cube 0 1 0.01)
  
   

(sum-cubes 1 10)

  
  (define (fib n)
    (fib-iter 1 0 n))
  
  
  (define (fib-iter a b count)
      (if(= count 0)
       b
      (fib-iter (+ a b) a (- count 1))))
  
  
  
 
       
        
              
      