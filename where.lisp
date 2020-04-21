

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

(defun where_multiply (statement separator)
  "split big query to list of single conditions"
  (let ((result '())
        (temp '()))

    (loop for arg in statement
          do (cond
               ((string= arg separator) (progn (setq result (append result (list (read-where temp))))
                                               (setq temp '())))
               (t (setq temp (append temp (list arg))))
               )
          )
    (setq result (append result (list (read-where temp))))
    (return-from where_multiply result)

    )
  )

(defun get_where (statement is_and is_or)
  "define separator (AND or OR)"
  (cond
    (is_and (where_multiply statement "and"))
    (is_or (where_multiply statement "or"))
    (t (list (read-where statement)))))

(defun init_id (list_where table-names)
  (let ((id-s '() ))

    (loop for w in list_where
          do (setq id-s (append id-s
                                (list  (is_column table-names
                                                  (read-from-string (where-statement-first_arg w)))))))
    (return-from init_id id-s)
    )
  )

(defun define_operations (list_where)
  (let ((operations '()))
    (loop for w in list_where
          do( if (not (eq (read-from-string (where-statement-second_arg w)) 0))
                 (setq operations (append operations
                                          (list  (get_operation
                                                 (where-statement-operation w)
                                                 (read-from-string (where-statement-second_arg w))))))
                 (setq operations (append operations (list
                                                      (get_operation
                                                       (where-statement-operation w)
                                                       (where-statement-second_arg w)))))


              ))
    (return-from define_operations operations)
    )
  )

(defun execute_condition ( rows condition id operation)
  "create table for one condition"
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (rows row)
      
      (if (funcall operation
                   (simple-table:get-row-column id row)
                   (read-from-string (where-statement-second_arg condition)))
          (simple-table:add-to-table row result)
          )
      )
    (return-from execute_condition result)
    )
  )

(defun get_tables (list_where id-s operations basic)
  "get tables that associate with certain condition "
  (let ((result '())
        (iterator 0))

    (loop for w in list_where
          do (progn
               (setq result (append result (list ( execute_condition basic w (nth iterator id-s) (nth iterator operations)) )))
               (setq iterator (+ iterator 1))))
    (return-from get_tables result)
    )
  )

(defun intersect (smaller_t static_t)
  "intersections of tables"
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (smaller_t small_row)
      (simple-table:with-rows (static_t static_row)
        (if (equal (coerce small_row 'list) (coerce static_row 'list)
             )
            (simple-table:add-to-table small_row result)
            )
        )      
      )
    (return-from intersect result)
    )
  )

(defun union_cond (base_el tables_list)
  "union tables with certain conditions"
  (cond
    ((null tables_list) base_el)
    (t (if (< (simple-table:num-rows base_el) (simple-table:num-rows (car tables_list)))
           (union_cond (intersect base_el (car tables_list)) (cdr tables_list))
           (union_cond (intersect (car tables_list) base_el) (cdr tables_list))))
    )
  )




(defun where (table-rows table-names statement is_and is_or)
  "implementation of where operation"
  (let ((result (simple-table:make-table))
        (id-s '())
        (operations)
        (list_where)
        (tables))
    ;; get list of conditions
    (setq list_where (get_where statement is_and is_or))

    ;; get id-s of column that would be compare
    (setq id-s (init_id list_where table-names))

    ;; get operations for all conditions
    (setq operations (define_operations list_where))

    ;; get tables that fits to one certain condition
    (setq tables (get_tables list_where id-s operations table-rows))

    ;; get result table by intersection of all tables that we have
    (setq result (union_cond (car tables) (cdr tables))) 
    
    (return-from where result)

    )  
  )
 
