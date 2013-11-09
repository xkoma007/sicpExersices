(define (money-kinds  change)
       (cc   change    5))


(define (cc total  num)
        
         (cond ((= total 0) 1)
               ( (or (< total 0) (= num 0) ) 0)
               (else ( + (cc total (- num 1)) (cc (- total  (f num)) num)))))


(define  (f num)
   (cond 
        ( (= num 5) 50)
          ( (= num 4) 25)
            ( (= num 3) 10)
              ( (= num 2) 5)
                ( (= num 1) 1)
                ))
         
                      
               





