(define (integral f a b n)
  
  (define h
    (/ (- b a) n) 
    )
  
  (define (koma  k )
    (* (simpson n k) (f (+ a (* k h)))))
  
  
  
  (define (next c) (+ 1 c))

(define (sum koma a next b  )
  (if (> a b)  
      0 
      (+(* (/ h 3) (koma a ) )
         (sum koma (next a) next b))))  
  
    (sum koma  0 next n )
  )


  
  (define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a)
                  (+ (term a) result))))
    (iter a 0))

(sum (lambda (x) x)
           1
           (lambda (i) (+ 1 i))
           10)

(define ( even? k)
  (=  (remainder k 2) 0 ))  

(define (simpson n k)
  (cond ( (or (= 0 k) (= k n)) 1)
        ((even? k) 2)
        (else 4)))

(define (cube x) (* x x x))

(integral cube (exact->inexact 0) 1 1000)