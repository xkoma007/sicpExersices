;  n行m个


(define (f n m)
 (cond 
       ( ( or (= m 0) (= m n) ) 1)
       ( else  (+ (f (- n 1) (- m 1)) (f (- n 1) m)))))

(f 4 0) (f 4 1) (f 4 2) (f 4 3)  (f 4 4)  