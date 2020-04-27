
(defun right_join (statement base)
  (let ((query (read_join statement))
        (proection)
        (result)
        (new_rows (simple-table:make-table))
        (b_rows)
        (p_rows)
        (inserted_row)
        (col_idp)
        (col_idb)
        (new_names))
    (setq proection (gethash (join-statement-right-sourse query) datasourse))
    (setq new_names (append (table-rows_names proection) (table-rows_names base)))


    (setq b_rows (table-rows base))
    (setq p_rows (table-rows proection))

    (if (string= (table-name base) (join-statement-sourse1 query))

        (progn
          (setq col_idp (car (get_col_ids (table-rows_names proection)
                                          (join-statement-name-col2 query) )))
          (setq col_idb (car (get_col_ids (table-rows_names base)
                                          (join-statement-name-col1 query) )))
          )

        (progn
          (setq col_idp (car (get_col_ids (table-rows_names proection)
                                          (join-statement-name-col1 query) )))
          (setq col_idb (car (get_col_ids (table-rows_names base)
                                          (join-statement-name-col2 query) )))

          )
        

        )
  

   
  

    (simple-table:with-rows (p_rows pro_row)
      (setq inserted_row (find_rows_all b_rows col_idb (aref pro_row col_idp)))
      (setq inserted_row (concatenate 'vector  pro_row inserted_row ))
 
      (simple-table:add-to-table inserted_row new_rows)         
      )

    
    
   
    (setq result (create_table new_rows "right-join" new_names))
    
    (return-from right_join result)
    )
  )
