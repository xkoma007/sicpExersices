;;;谓词检测


(load "4.1.2.scm")

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


;;过程的表示

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-enviroment p) (cadddr p))
 
;;;对环境的操作


(define (enclosing-enviroment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-enviroment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (extend-enviroment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (errror "Too few arguments supplied" vars vals))))



(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-enviroment env)))
             ((eq? var (car vars))
              (car vals))
             (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-varibales frame)
                (frame-values frame)))))
  (env-loop env))



(define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-enviroment env)))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable --- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-varibales frame)
                (frame-values frame)))))
  (env-loop env))



;;;
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

            
      
    
    
           
                     
