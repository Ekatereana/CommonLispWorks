
(defun get_col_ids (columns query)
  "get ids of columns that new table should contain"
  (let ((result '()))
    (loop for col in query
          do
          (if (position (read-from-string col) columns)
              ( setq result
                     (append result (list  (position (read-from-string col) columns))))
              ))
    (return-from get_col_ids result))
  )


(defun make_query_table (basic columns distinct condition is_and is_or)
  "build table for query"
  (let ((col_ids);; get numbers of columns that should be displayed in new table
        (new_rows (simple-table:make-table)) ;; create new table
        (row)
        (b_rows)
        (new_names '()))

    
    ;;distinct
    (if distinct
        ;; if should build new table with unique values
        (progn (setq col_ids (get_col_ids (table-rows_names basic) (cdr columns)))
               (setq b_rows (distinct (table-rows basic))))
        (progn (setq col_ids  (get_col_ids (table-rows_names basic) columns))
               (setq b_rows (table-rows basic))))
    
    ;;new column names for table
    (loop for id in col_ids
          do (setq new_names (append new_names (list (nth id (table-rows_names basic))))))
    
    ;; where
    (if condition
        (setq b_rows (where b_rows (table-rows_names basic) condition is_and is_or) ))
    ;;(print (where b_rows (table-rows_names basic) condition))   


    (simple-table:with-rows (b_rows row_b)
      (setq row (simple-table:make-row));; initialize new row
      (loop for id in col_ids;; for all columns that will be used
            do (simple-table:add-to-row ;; add to new row
                (simple-table:get-row-column id row_b) row))
      
      (simple-table:add-to-table row new_rows) ;; add new row to table
      )
    
    
    (return-from make_query_table (create_table new_rows (table-name basic)
                                                new_names));; return instance of table-structure
    )
  )
