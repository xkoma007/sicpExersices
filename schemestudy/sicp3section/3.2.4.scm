

;;; 内部定义

;;; test  3.11
(make-account 50) 状态保存在E1中，E1的外围环境为全局环境。
acc 被约束为 E1的过程对象deposit中，而该过程对象则指向E1环境变量。
acc2 被约束为E2的过程对象deposit中，而该过程对象指向E环境变量。
两部分共用的结构为make-account 过程对象。



