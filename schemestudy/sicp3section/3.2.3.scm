

(define (make-withdraw balance)
      (lambda (amount)
	(if (>= balance amount)
	    (begin (set! balance (- balance amount))
		        balance)
	    "Insufficient funds")))

(define W1 (make-withdraw 100))
(W1 50)

;;; 3.10
一样的，唯一的区别是，多了一个局部环境变量 balance，原来的结构仅使用形式参数
initial-amount 来维持局部变量状态即可。
