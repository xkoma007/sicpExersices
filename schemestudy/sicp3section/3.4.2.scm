
;控制并发的机制

;对共享变量的串行访问
(load "parallel.scm")

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
				  
				
(display x)
(newline)
(display x)
(newline)
(display x)
(newline)

(define x 10)
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (+ x 1)))))


(display x)
(newline)
(display x)
(newline)
(display x)
(newline)
(display x)


;test 3.39
; (define x 10)
; (define s (make-serializer))
  (parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                    (s (lambda () (set! x (+ x 1)))))
; 1. 101,p1 执行完,p2 在执行.
; 2. 121,p2 执行完,p1 在执行.
; 3. 11, p1 访问x两次, 在还没有进行赋值的时候,p1 先赋值. 然后p2在进行赋值.
; 4. 100, p1 访问x两次, 在还没有进行赋值的时候,p2 先赋值. 然后p1在进行赋值.

; test 3.40

; 未串行化之前
;1. 1000000,p1 取第一个x之时p2执行完 或者 p2 取第一个x时p1执行完 .
;2. 10000,p1 取第一个x之时p2执行完 或者 10000,p2 取第三个x时p1执行完.
;3. 1000,p1 设置后,变成p2执行完.
;4. 100,p1 设置值前p2执行完.
;6. 100000,p2 取第二个x时p1执行完.

; 串行化之后
; 1. 1000000,p1 执行完,p2 再执行 或者 p2 执行完,p1 再执行.
;


; test 3.41
; 看到的金额值可能不正常,Ben 的担心很有必要,但实际运行取款,存款没有问题,我们看到的结果最终也是正确的.

; test 3.42
; 安全,这样修改 我们序列化管理的过程不会因为每次账户访问操作而增加新的过程,这样程序处理效率会更高.


;使用多重共享资源的复杂性


;test 3.43
;test 3.44
;test 3.45

;串行化的实现

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
       (mutex 'acquire)
       (let ((val (apply p args)))
          (mutex 'release)
           val))
    serialized-p)))
          
;;死锁


;Scheme里的串行化
