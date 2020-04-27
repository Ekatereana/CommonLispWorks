(defstruct join-statement
  sourse1
  name-col1
  sourse2
  name-col2
  right-sourse)

(defun read_join (statement)
  (let ((parse_head (split (nth 1 statement) #\.))
        (parse_back (split (nth 3 statement) #\.)))
    (make-join-statement
     :sourse1 (car parse_head )
     :name-col1 (cdr parse_head )
     :sourse2 (car parse_back)
     :name-col2 (cdr parse_back)
     :right-sourse (car statement)
     )
    )
  )

(defun find_some (table1 table2 operation)
  (cond
    ((funcall operation (simple-table:num-rows table1)  (simple-table:num-rows table2)) table1)
    (t table2))
  )

(defun find_row_by_column (table col_id value)
  (simple-table:with-rows (table row)
    (if (funcall (get_operation "=" value) value (aref row col_id))
        (return-from find_row_by_column row)
    )
  )
)


(defun inner_join (statement base)
  (let ((query (read_join statement))
        (proection)
        (result)
        (new_rows (simple-table:make-table))
        (b_rows)
        (s_rows)
        (inserted_row)
        (col_ids)
        (col_idb)
        (new_names))
    (if (string= (table-name base) (join-statement-sourse1 query))
        (setq proection (gethash (join-statement-sourse1 query) datasourse))
        (setq proection (gethash (join-statement-sourse2 query) datasourse))      
      
        )

   


    (setq b_rows (find_some (table-rows proection)  (table-rows base) #'>))
    (setq s_rows (find_some (table-rows base) (table-rows proection) #'<))

    (if (eq (table-rows base) b_rows)
        (progn
          (setq col_idb (car (get_col_ids (table-rows_names base)
                                          (join-statement-name-col1 query) )))
          (setq col_ids (car (get_col_ids (table-rows_names proection)
                                          (join-statement-name-col2 query) )))
          (setq new_names (append  (table-rows_names base) (table-rows_names proection)))
          )
        (progn
          (setq col_ids (car (get_col_ids (table-rows_names proection)
                                          (join-statement-name-col1 query) )))
          (setq col_idb (car (get_col_ids (table-rows_names base)
                                          (join-statement-name-col2 query) )))
          (setq new_names (append (table-rows_names proection) (table-rows_names base)))
          
        )     
    
        )

    (simple-table:with-rows (s_rows smaller_row)
      (setq inserted_row (find_row_by_column b_rows col_idb (aref smaller_row col_ids)))
      (setq inserted_row (concatenate 'vector inserted_row smaller_row  ))
      (simple-table:add-to-table inserted_row new_rows)         
      )
    (setq result (create_table new_rows "inner-join" new_names))
    (return-from inner_join result)
    )
  )
 

        
