
(defun min_f (rows id)
  (let ((min_result (simple-table:get-row-column id (simple-table:get-row 0 rows))))
    (simple-table:with-rows (rows row)
      (if (> min_result (simple-table:get-row-column id row))
          (setq min_result (simple-table:get-row-column id row))))
    (return-from min_f min_result)
    )
  )

(defun avg (rows id)
  (let ((avg_result 0))
    (simple-table:with-rows (rows row)
      (setq avg_result (+ avg_result  (simple-table:get-row-column id row)))
      )
    (setq avg_result (floor avg_result (simple-table:num-rows rows )))
    (return-from avg avg_result)
    )
  )

(defun get_function ( name)
  (cond
    ((string= name "avg") #'avg)
    ((string= name "min") #'min_f)))

(defun split_on_list_of_tables (table id)
  (let ((result '())
        (temp (simple-table:make-table))
        (value (aref (aref table 0) id))
        (operation))

    (setq operation (get_equal value))
    (simple-table:with-rows (table row)
      (if (funcall operation value (aref row id))
          (setq temp (simple-table:add-to-table row temp))
          (if temp 
              (progn
                (setq value (aref row id))
                (setq result (append result (list temp)))
                (setq temp (simple-table:make-table))
                (setq temp (simple-table:add-to-table row temp)) ))

          )

      )
    (return-from split_on_list_of_tables result)
    )

  )

(defun iterate_on_tables (tlist function id )
  
  (let ((result (simple-table:make-table))
        (inserted_v)
        (inserted_row))
    (loop for rows in tlist
          do
          (setq inserted_v (funcall function rows id))
          (setq inserted_row (make-array (length (aref rows 0)) :initial-contents
                                         (coerce (aref rows 0) 'list)))
          (setf (aref inserted_row id) inserted_v)  
          (simple-table:add-to-table inserted_row result)
         )

    (return-from iterate_on_tables result)
    
 
    )


  )

(defun create_result_of_function (rows id name is-all &optional group-by)
  (let ((result (funcall (get_function  name) rows id))
        (new_r (simple-table:make-table))
        (n_row))
    (if is-all
        (if group-by
            (setq new_r (iterate_on_tables
                          (split_on_list_of_tables rows (car group-by))
                          (get_function name)
                          id))
            (progn  (simple-table:with-rows (rows row)
                      (setq n_row (make-array (length row) :initial-contents (coerce row 'list)))
                      (setf (aref n_row id) result)  
                      (simple-table:add-to-table n_row new_r)
                      
                      )))
        (progn
          (setq n_row (make-array (length (aref rows 0)) :initial-contents
                                  (coerce (aref rows 0) 'list)))
          (setf (aref n_row id) result)  
          (simple-table:add-to-table n_row new_r))
        )
     
    (return-from create_result_of_function new_r)
    )
  )




