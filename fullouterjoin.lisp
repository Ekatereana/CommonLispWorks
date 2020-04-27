

(defun find_rows_all (table col_id value)
  (let ((len))

    (simple-table:with-rows (table row)
      (setq len (length row))
      (if (funcall (get_operation "=" value) value (aref row col_id))
          (return-from
           find_rows_all row )
          
          )
      
      )
    (return-from find_rows_all (make-array len :initial-element nil))
    ) 
  )

(defun full_outer_join (statement base)
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
          (setq new_names (append  (table-rows_names base)
                                   (table-rows_names proection)))
          )
        (progn
          (setq col_ids (car (get_col_ids (table-rows_names proection)
                                          (join-statement-name-col1 query) )))
          (setq col_idb (car (get_col_ids (table-rows_names base)
                                          (join-statement-name-col2 query) )))
          (setq new_names (append (table-rows_names proection)
                                  (table-rows_names base)))
          
          )     
        
        )


      

      (simple-table:with-rows (b_rows bigge_row)
      (setq inserted_row (find_rows_all s_rows col_ids (aref bigge_row col_idb)))
      (setq inserted_row (concatenate 'vector  bigge_row inserted_row ))
      (simple-table:add-to-table inserted_row new_rows)         
        )
    
   
    
    (setq result (create_table new_rows "outer-join" new_names))
    
    (return-from full_outer_join result)
    )
  )
