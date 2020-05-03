






(defun create_mini_table (table value id)
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (table row)
      (if (funcall (get_equal (aref row id)) (aref row id) value)
          (setq result (simple-table:add-to-table row result)))

      )
    (return-from create_mini_table result)
    )
  )

(defun union_groups (table id)
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (table row)
      (if (not (in_table result row id))
          (setq result (concatenate 'vector result (create_mini_table table (aref row id) id)))
          )
      )
    (return-from union_groups result)
    )

  )

(defun group_by (table id &optional (agregate_functions '()))
  (let ((result (simple-table:make-table)))
    (if agregate_functions
        (setq result (union_groups table (car id)))
        (progn
          (setq result (distinct table (car id)))))
    
    (return-from group_by result)
    )
    )
