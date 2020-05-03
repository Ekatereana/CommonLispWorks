
(defun side_join (statement base is_right)
  (let ((query (read_join statement))
        (proection)
        (result)
        (new_rows (simple-table:make-table))
        (b_rows)
        (p_rows)
        (inserted_row)
        (col_idp)
        (col_idb)
        (new_names)
        (table-name))


    (if is_right

        (progn
          (setq proection (gethash (join-statement-right-sourse query) datasourse))
          (setq new_names (append (table-rows_names proection) (table-rows_names base)))
          )
        (progn
          (print "left")
          (setq proection (gethash (join-statement-right-sourse query) datasourse))
          (setq new_names (append  (table-rows_names base) (table-rows_names proection)))

          )

        )

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
  

  

    (if is_right

        (progn
          (simple-table:with-rows (p_rows pro_row)
                  (setq inserted_row (find_rows_all b_rows col_idb (aref pro_row col_idp)))
                  (setq inserted_row (concatenate 'vector  pro_row inserted_row ))
                  
                  (simple-table:add-to-table inserted_row new_rows)         
            )
          
          (setq table-name "right-join")
          
                )
        (progn
          (simple-table:with-rows (b_rows b_row)
            (setq inserted_row (find_rows_all p_rows col_idp (aref b_row col_idb)))
            (setq inserted_row (concatenate 'vector  b_row inserted_row ))
            
            (simple-table:add-to-table inserted_row new_rows)         
            )
          (setq table-name "left-join")
          )

        )
  
    
   
    (setq result (create_table new_rows table-name  new_names))
    
    (return-from side_join result)
    )
  )
