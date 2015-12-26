;;#lang planet neil/sicp

;;一维表格

;(define (assoc key records)
;  (cond ((null? records) false)
 ;       ((equal? key (caar records)) (car records))
 ;       (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
      (if record
          (cdr record)
          false)))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)



;(define (make-table)
 ; (list '*table*))


;二维表格


;局部表格



(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2 )
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert1 key-1 key-2 value)
  (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable) same-key?)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable 
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! local-table 
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else 'ok)))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))
    

;3.24
;直接在上面的事例中修改了


;3.25
; 中文版翻译有错
; 在这里我们需要实现一个多维表格来参数keys将是一个序列，用来描述多个关键码。

(define (make-dimensions-table)
  (let ((local-table (list '*table*)))
    (define (lookup keys current-table)
      (let ((subtable (assoc (car keys) (cdr current-table))))
        (if subtable
            (if (cdr keys)
                (lookup (cdr keys) subtable)
                (cdr subtable))
            false)))
    (define (insert! keys value current-table)
      (let ((subtable (assoc (car keys) (cdr current-table))))
        (if subtable
            (if (cdr keys)
                (insert! (cdr keys) value subtable)
                (set-cdr! subtable value))
            (if (cdr keys)
                (let ((next-table (cons (car keys) '())))
                  (set-cdr! current-table (cons next-table (cdr current-table)))
                  (insert! (cdr keys) value next-table))  
                (set-cdr! current-table (cons (cons (car keys) value)
                                              (cdr current-table)))))))
    'ok))
        


;3.26
;3.27

 
    
    
