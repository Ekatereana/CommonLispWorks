(defstruct having-statement
  func
  col
  operation
  value)

(defun read-having (statement)
  (make-having-statement
   :func (nth 0 statement)
   :col (car (get_args statement "(" ")"))
   :operation (nth 4 statement)
   :value (nth 5 statement)))


(defun having (table tnames statement)
  (let ((query (read-having statement))
        (result)
        (for-where '()))
    (setq for-where (list (having-statement-col query)
                          (having-statement-operation query)
                          (having-statement-value query)))
    (setq result (where table tnames for-where))
    
    )
  )
