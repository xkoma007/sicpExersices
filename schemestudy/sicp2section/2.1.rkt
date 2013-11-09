(define (add-rat x y)
    (make-rat (+ (* (number x) (denom y)) 
                 (* (number y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
        (make-rat (- (* (number x) (denom y))
                     (* (number y) (denom x)))
                  (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (number x) (number y))
              (* (denom x)  (denom y))))
(define (div-rat x y)
        (make-rat (* (number x) (denom y))
                  (* (denom x)  (number y))))


(define (equal-rat? x y)
  (= (* (number x) (number y))
     (* (number y) (denom x))))

(define (make-rat n d) (cons n d))
(define (number x) (car x))
(define (denom x) (cdr x))
 
 (define (print-rat x)
     (newline)
     (display (number x))
     (display "/")
     (display (denom x)))
 
 (define (neg d)
    (- 0 d)) 
 (define (make-rat n d)
  (let ((g (gcd n d)))
       ( cond      ( (and ( < n 0) (< d 0))
                  (cons (/ (neg n) g) (/ (neg d) g)))
                 ((and ( > n 0) (< d 0))
                 ( cons (/ (neg n) g) (/ (neg d) g)))
                 (else (cons (/ n g) (/ d g))))))

(define (gcd a b)
  ( if (= b 0)
      (if (< a 0) (neg a) a) 
       (gcd b (remainder a b))))
 
 (define one-half (make-rat -1 2))
  (print-rat one-half)
  
  (define one-third (make-rat 1 -3))
  
  (print-rat (add-rat one-half one-third))
  (print-rat (mul-rat one-half one-third))
  (print-rat (add-rat one-third one-third))
  
  
(gcd -2 -3)
    