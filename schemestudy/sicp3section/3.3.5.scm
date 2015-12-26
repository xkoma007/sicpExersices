;约束的传播
(load "3.3.4.scm")
(define C (make-connector))
(define F (make-connector))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
       (multiplier c w u)
       (multiplier v x u)
       (adder v y f)
       (constant 9 w)
       (constant 5 x)
       (constant 32 y)
       'ok))
       

;约束系统的使用
;约束系统的实现
;连接器的表示
