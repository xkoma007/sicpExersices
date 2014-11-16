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
      
      