(define (product factorial a next b)
         (if (> a b)
             1
             (* (factorial a)  (product factorial (next a) next b))))

(define (next s)
 (+ s 2))




(define (factorial s)
  (let ( (y (square s) ))
       (/ (- y 1)  (exact->inexact y))))
        
 
      

(define (square s)
  (* s s))


   (define
     (product1 factorial a next b result)
        (cond ( (> a b) result)
             (else (product1 factorial (next a) next b (* result (factorial a))))))
     
   
  
 
 ;(* 4  (product factorial 3 next 10000000000000000))
 (* 4  (product1 factorial 3 next 100000000 1))
 
(define (accumulate combiner null-value term a next b)
    ( if( > a b)  null-value
     (combiner (term a) (accumulate combiner null-value term (next a) next b) )))

(define (combiner a b)
  (* a b))

(* 4 (accumulate combiner 1 factorial 3 next 100))
;(define (filtered-accumulate combine null-value term a next b predicate)