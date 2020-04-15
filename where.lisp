

(defun get_operation (symbol value)
  (cond
    ((string= symbol "=")
     (cond
       ((numberp value) #'=)
       ((stringp value) #'string=)))
    ((string= symbol ">")
     (cond
       ((numberp value) #'>)
       ((stringp value) #'string>)))
    ((string= symbol "<")
     (cond
       ((numberp value) #'<)
       ((stringp value) #'string<)))
    (t #'eq)
    )
  )


(defstruct where-statement
  first_arg
  second_arg
  operation
  
  )

(defun is_column (row column)
  (position column row))

(defun read-where (stat)
  (let ((statement (coerce stat 'vector)))
    
    (make-where-statement
     :first_arg (aref statement 0)
     :second_arg (aref statement 2)
     :operation (aref statement 1)
     ))
  )



(defun where (table-rows table-names statement)
  (let ((condition (read-where statement))
        (result (simple-table:make-table))
        (id)
        (operation))
    (setq id
          (is_column table-names
                     (read-from-string (where-statement-first_arg condition))))
    (if (not (eq (read-from-string (where-statement-second_arg condition)) 0))
        (setq operation
              (get_operation
               (where-statement-operation condition)
               (read-from-string (where-statement-second_arg condition))))
        (setq operation
              (get_operation
               (where-statement-operation condition)
               (where-statement-second_arg condition)))
        )
    
    
    (simple-table:with-rows (table-rows row)
      (if (funcall operation
                   (simple-table:get-row-column id row)
                   (read-from-string (where-statement-second_arg condition)))
          (simple-table:add-to-table row result)
          )
      )
    (return-from where result)
    )
  )
