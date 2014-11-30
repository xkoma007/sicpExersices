(define (f n)
 (cond ( (< n 3) n)
    ( else  (+ (f (- n 1))  (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))  ;递归式





(define (h n)
  (cond ( (< n 3) n)
        ( else (g  n 3 2 1 0 )
           )))


(define   (g n k a b c)
   
  (cond ( (> n k)   (g n (+ k 1)  (+ a (* 2 b)  (* 3 c) ) a b))
                    
        ( else  (+ a (* 2 b)  (* 3 c)))))
       
  
  
         



