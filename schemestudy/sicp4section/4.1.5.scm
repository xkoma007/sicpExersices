;将数据作为程序

(define the-global-environment user-initial-environment)

(eval '(* 5 5) user-initial-environment)
(eval (cons '* (list 5 5)) user-initial-environment)
