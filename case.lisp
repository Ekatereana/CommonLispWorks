(defstruct case-statement
  id
  condition
  action
  )

(defun merge_list (list)
  (let ((result ""))

    (loop for el in list
          do
          (if (eq (length result) 0)
              (setq result (concatenate 'string result  el ))
              (setq result (concatenate 'string result " " el )))
          )
    (return-from merge_list result)
    )
  )


(defun read-case-statement (statement id)
  (make-case-statement
   :id id
   :condition (read-where (get_args statement "when" "then"))
   :action (merge_list (get_args statement "then"))
   )
  )

(defun case_multiply (statement)
  (let ((result '())
        (temp '())
        (iterator 0)
        (id_iter 0))
    
    (loop while (< iterator (length statement))
          do
          
          (if
           (and (string= (nth iterator statement) "when")
                (position "when" temp :test #'string=))
              (progn
                (setq result (append result (list (read-case-statement temp id_iter))))
                (setq id_iter (+ 1 id_iter))
                (setq temp '())
                (setq iterator (- iterator 1)))
              (setq temp (append temp (list (nth iterator statement)))))
          (setq iterator (+ 1 iterator))        
          )
    (setq result (append result (list  (read-case-statement temp id_iter))))
    (return-from case_multiply result)

    )
  )

(defun inspect_rows (rows names case-el)
  (let ((result (simple-table:make-table))
        (operation)
        (id)
        (condition))
    (setq condition (case-statement-condition case-el))

    (if (eq (read-from-string (where-statement-second_arg condition)) 0)
        (setq operation (get_operation (where-statement-operation condition)
                                       (where-statement-second_arg condition)))
        (setq operation (get_operation (where-statement-operation condition)
                                       (read-from-string (where-statement-second_arg condition))))



        )
    
    (setq id (get_col_ids names  (list (where-statement-first_arg condition))))

    (simple-table:with-rows (rows row)
      
      (if (funcall operation
                   (simple-table:get-row-column (car id) row)
                   (read-from-string (where-statement-second_arg condition)))
          (simple-table:add-to-table row result)
          )
      )
    
    (return-from inspect_rows result)
    )

  )


(defun get_ctables (table names clist)
  (let ((result '()))

    (loop for el in clist
          do
          (setq result (append result (list (inspect_rows table names el)))))
    


    (return-from get_ctables result)
    )

  )

(defun find_table_position (tlist row)
  (let ((iterator 0))
    (loop while (< iterator (length tlist))
          do
          (if (in_table (nth iterator tlist) row)
              (return-from find_table_position iterator))
          (setq iterator (+ 1 iterator)))
    )

  )




(defun format_new_table (table clist ctables)
  (let ((result (simple-table:make-table))
        (position)
        (inserted))
    (simple-table:with-rows (table row)
      (setq position (find_table_position ctables row))
      (setq inserted (make-array (length row) :initial-contents (coerce row 'list)))
      (if position
          (progn           
            (setq inserted (concatenate 'vector inserted (list
                                                          (case-statement-action (nth position clist))))))
          (setq inserted (concatenate 'vector inserted (list nil)))
          )
      (setq result (simple-table:add-to-table inserted result))
      )

    

    (return-from format_new_table result)
    )

  )



(defun case_end (table names statement)
  (let ((result (simple-table:make-table))
        (clist '())
        (tlist '()))
    (setq clist (case_multiply statement))
    (setq tlist (get_ctables table names clist))
    (setq result (format_new_table table clist tlist))
  
    (return-from case_end result)
    )
)
