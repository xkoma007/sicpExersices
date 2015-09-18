  #lang racket
 (require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1))) 
 ;;(paint einstein)

(define einstein2
   (beside einstein (flip-vert einstein)))

(define einstein4
        (below einstein2 einstein2))

(define (flipped-pairs painter)
  (let ((painter2  (beside painter (flip-vert painter))))
       (below painter2 painter2)))

(define einstein5
  (flipped-pairs einstein))

;(define (right-split painter n)
   ; (if (= n 0)
     ;   painter
       ; (let ((smaller (right-split painter (- n 1))))
        ;  (beside painter (below smaller smaller)))))



;;;;test 2.44
;(define (up-split painter n)
  ;  (if (= n 0)
     ;   painter
      ;  (let ((smaller (up-split painter (- n 1))))
        ;  (below  painter (beside smaller smaller)))))

   
;;;;;;;;;test 2.45
(define 
   (split fh sh) 
  (lambda (identity n)
    (if (= n 0)
        identity
        (let ((smaller ((split fh sh) identity (- n 1))))
        (fh identity (sh smaller smaller))))))
        
     (define right-split (split beside below))
     (define up-split (split below beside))
     

(define  (corner-split painter n)
        (if  (= n 0)
              painter
              (let ((up (up-split painter (- n 1)))
                (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
         (let ((quarter (corner-split painter n)))
              (let ((half (beside (flip-horiz quarter) quarter)))
                (below (flip-vert half) half))))

(define (square-of-four t1 tr b1 br)
  (lambda (painter)
         (let ((top (beside (t1 painter) (tr painter)))
                (bottom (beside (b1 painter) (br painter))))
           (below bottom top))))



(paint (square-limit einstein 4))    
(paint einstein)           
              

      
     
  