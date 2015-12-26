

;;; 队列的表示
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)  (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))


(define (front-queue queue)
              (if  (empty-queue? queue)
		   (error "FRONT called with an empty queue" queue)
		   (car (front-ptr queue))))



(define (insert-queue! queue item)
     (let ((new-pair (cons item '())))
        (cond  ((empty-queue? queue)
		     (set-front-ptr! queue new-pair)
		     (set-rear-ptr! queue new-pair)
		     queue)
	       (else 
		  (set-cdr! (rear-ptr queue) new-pair)
		  (set-rear-ptr! queue new-pair)
		  queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
	        (error "DELETE! called with an empty queue" queue))
	     (else 
	        (set-front-ptr! queue (cdr (front-ptr queue)))
	       queue)))



(define  q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(insert-queue! q1 'c)

(delete-queue! q1 )
(delete-queue! q1 )


;test 3.21
;这里queue队列序列只指向了序列的头指针和尾指针
(define (print-queue queue)
  (front-ptr queue) 
     )
(print-queue q1)


;test 3.22

(define (make-queue1)
  (let ((front-ptr (list '() '()))
        (rear-ptr (list '() '())))  
    (define (insert-queue! item)
     (let ((new-pair (cons item '())))
       (cond  ((null? (car front-ptr))
               (set-car! front-ptr new-pair)
               (set-car! rear-ptr new-pair)
               front-ptr)
              (else 
               (set-cdr! (car rear-ptr) new-pair)
               (set-car! rear-ptr new-pair)
               front-ptr))))
    (define (delete-queue!)
      (cond ((null? front-ptr)
             (error "DELETE! called with an empty queue" front-ptr))
            (else 
             (set-car! front-ptr (cdr (car front-ptr))))
             )
      front-ptr)
    (define (dispatch m)
      (cond
        ((eq? m 'insert-queue!)
         insert-queue!)
        ((eq? m 'delete-queue!)
         delete-queue!)
        (else (error "Unknown request"))
      ))
    dispatch))


(define queue-handle (make-queue1))
((queue-handle 'insert-queue!) 'a)
((queue-handle 'insert-queue!) 'b)
((queue-handle 'insert-queue!) 'c)
((queue-handle 'delete-queue!)) 

;test 3.23
