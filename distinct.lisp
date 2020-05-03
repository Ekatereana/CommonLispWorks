;; distinct function

(defun in_table (table is &optional id)
  "is row in table"
  (simple-table:with-rows (table row)
    (if id
        (if (funcall (get_equal (aref row id)) (aref row id) (aref is id))
            (return-from in_table t))
        (progn
          (if (equal (coerce row 'list) (coerce is 'list))
              (return-from in_table t))))
    )
  )

(defun distinct (table &optional id)
  "create new table with unique values"
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (table row)
      (if (not (in_table result row id))
          (simple-table:add-to-table row result))
      )
    (return-from distinct result)
    )
  )
