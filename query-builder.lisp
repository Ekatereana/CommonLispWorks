

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


(defun make_query_table (input_table
                         columns
                         distinct
                         condition
                         is_and
                         is_or
                         order-by
                         order-way
                         group-by
                         having
                         func
                         args_f
                         inner-join
                         full-outer-join
                         right-join
                         left-join)
  "build table for query"
  (let ((col_ids);; get numbers of columns that should be displayed in new table
        (new_rows (simple-table:make-table)) ;; create new table
        (row)
        (b_rows (simple-table:make-table))
        (new_names '())
        (col_order)
        (basic))

    (cond
      ((not (or columns args_f)) nil)
      (t (progn

           
           ;; inner join or full outer join
           (cond
             (inner-join  (setq basic (inner_join inner-join input_table)))
             (full-outer-join (setq basic (full_outer_join full-outer-join input_table)))
             (right-join (setq basic (side_join right-join input_table T)))
             (left-join (setq basic (side_join left-join input_table NIL)))
             (t (setq basic input_table)))
          
          

           (if args_f
               (setq col_ids (get_col_ids (table-rows_names basic) args_f)))
           
           (if columns
               (setq col_ids  (append col_ids (get_col_ids (table-rows_names basic) columns))))

     

           (setq b_rows (copy-seq (table-rows basic)))
           ;;group by
           (if group-by
               (setq b_rows
                     (group_by b_rows
                               (get_col_ids (table-rows_names basic) group-by)
                               (get_col_ids (table-rows_names basic) group-by))))

       
           
           ;;function
           (if func
               (cond
                 (group-by
                  (setq b_rows
                        (create_result_of_function
                         b_rows
                         (car col_ids)
                         func
                         T
                         (get_col_ids (table-rows_names basic) group-by))) )
                 (t (if (< (length col_ids) 2)
                        (setq b_rows (create_result_of_function b_rows (car col_ids) func NIL))
                        (setq b_rows (create_result_of_function b_rows (car col_ids) func T))
                        )))
               )

           
           ;;distinct
           (if distinct
               ;; if should build new table with unique values
               (setq b_rows (distinct b_rows))
               )
           
           ;;new column names for table
           (loop for id in col_ids
                 do
                 (if (position id (get_col_ids (table-rows_names basic) args_f))
                     (setq new_names (append new_names (list
                                                        (symbol-append (read-from-string
                                                                        (concatenate  'string func "_"))
                                                                       (nth id
                                                                            (table-rows_names basic))))))
                     (setq new_names (append new_names (list (nth id (table-rows_names basic)))))
                     )

                 )
        
           
           ;; where
           (if condition
               (setq b_rows (where b_rows (table-rows_names basic) condition is_and is_or) ))
           ;;(print (where b_rows (table-rows_names basic) condition))

          
           ;;having
           (if having
               (setq b_rows (having b_rows (table-rows_names basic) having)))


           ;;order by
           (if order-by
               (progn (setq col_order (get_col_ids (table-rows_names basic) order-by))
                      (setq b_rows (order_by order-way (car col_order) b_rows))))

           

           (simple-table:with-rows (b_rows row_b)
             (setq row (simple-table:make-row));; initialize new row
             (loop for id in col_ids;; for all columns that will be used  
                   do  (simple-table:add-to-row ;; add to new row
                       (simple-table:get-row-column id row_b) row))
             
             (simple-table:add-to-table row new_rows) ;; add new row to table
             )
          
           
           (return-from make_query_table (create_table new_rows (table-name basic)
                                                       new_names));; return instance of table-structure

           )
         )
      )
    )
  )
