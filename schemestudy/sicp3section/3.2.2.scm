

;;;  环境变量，这里普通的代换模型将被替换为复杂的环境模型
;;;  简单过程的应用

(define (square x)
   (* x x))

(define (sum-of-squares x y)
  (+  (square x) (square y)) )

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))


;;;  test 3.9

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;; 递归版本

函数定义 构建一个新的环境a1，过程体 factorial将约束整个过程对象 ，
该环境的外围环境为全局环境。
(factorial 6)创造新的环境e1, 6 约束与形式参数n，外围环境属于 过程对象a1
(factorial 5)创造新的环境e2, 5 约束与形式参数n，外围环境属于e1
(factorial 4)创造新的环境e3, 4 约束与形式参数n，外围环境属于e2
(factorial 3)创造新的环境e4, 3 约束与形式参数n，外围环境属于e3
(factorial 2)创造新的环境e5, 2 约束与形式参数n，外围环境属于e4
(factorial 1)创造新的环境e6, 1 约束与形式参数n，外围环境属于e5
;;; 迭代版本 略。

