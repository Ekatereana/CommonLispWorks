

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

(defun get_function (rows id name)
  (cond
    ((string= name "avg") ( avg rows id))
    ((string= name "min") ( min_f rows id))))

(defun create_result_of_function (rows id name is-all)
  (let ((result (get_function rows id name))
        (new_r (simple-table:make-table))
        (n_row))
    (if is-all
        (simple-table:with-rows (rows row)
          (setq n_row (make-array (length row) :initial-contents (coerce row 'list)))
          (setf (aref n_row id) result)  
          (simple-table:add-to-table n_row new_r)
          
          )
        (progn
          (setq n_row (make-array (length (aref rows 0)) :initial-contents
                                  (coerce (aref rows 0) 'list)))
          (setf (aref n_row id) result)  
          (simple-table:add-to-table n_row new_r))
        )
     
    (return-from create_result_of_function new_r)
    )
  )




