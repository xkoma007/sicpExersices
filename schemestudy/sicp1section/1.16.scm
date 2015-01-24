;1.16尾递归
(define (fast-expt b n)
   (expt-iter b n 1))
(define (expt-iter b n a)
   (cond ( (= n 0) a)
         ( (even? n) (expt-iter (* b b) (/ n 2) a))
         (  (odd? n)  (expt-iter b (- n 1) (* a b)))))
(define (even? n)
    (= ( / n 2.0) 0))
(define (odd? n)
   (not (= ( / n 2.0) 0)))
;1.17


(define (muti a b)
 ( cond 
        ((= b 0) 0)
        ( (even? b) ( muti (double a) (halve b)) )
        ( (odd? b)  (+ a (muti a (- b 1) ) ))))

(define (double n)
  (+ n n))

(define (halve n)
  (cond (even? n)  (/ n 2)))
     
      (muti 8 10)
      


