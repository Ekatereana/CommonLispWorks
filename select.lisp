;; function select

(defstruct select-statement
  columns
  is_distinct
  sourse
  condition
  limit
  )

(defun read-select (clean_list)
  "read parts of select statement"
  (make-select-statement
   :columns (get_args clean_list "select" "from")
   :is_distinct (string= "distinct"
                         (car (get_args clean_list "select" "from")) )
   :sourse (get_args clean_list "from" "where")
   :condition (get_args clean_list "where" "limit")
   :limit (get_args clean_list "limit")
   )  
  )

(defun get_col_ids (columns query)
  "get ids of columns that new table should contain"
  (let ((result '()))
    (loop for col in query
          do(setq result (append result (list  (position (read-from-string col) columns)))))
    (return-from get_col_ids result)
    )
  
  )

(defun make_query_table (basic columns)
  "build table for query"
  (let ((col_ids (get_col_ids (table-rows_names basic) columns));; get numbers of columns that should be displayed in new table
        (new_rows (simple-table:make-table)) ;; create new table
        (row))
    
    (simple-table:with-rows ((table-rows basic) row_b)
      (setq row (simple-table:make-row));; initialize new row
      (loop for id in col_ids;; for all columns that will be used
            do (simple-table:add-to-row ;; add to new row
                (simple-table:get-row-column id row_b) row))
      (simple-table:add-to-table row new_rows) ;; add new row to table
      )
    
    (return-from make_query_table (create_table new_rows));; return instance of table-structure
    )
  )


(defun select (statement)
  (let (
        (query (read-select (clean-list (split statement))))
        (table)
        )
    ;; identify table that we use.    
    (setq table (gethash (car (select-statement-sourse query)) datasourse))
    (cond
      ((null table) (print "ERROR::: Unknown table, please try again"))
      ((string= (car (select-statement-columns query)) "*") (pretty_view table))
      (t (pretty_view (make_query_table table (select-statement-columns query))))
      )
    ))
