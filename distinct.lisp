;; distinct function

(defun in_table (table is)
  "is row in table"
  (simple-table:with-rows (table row)
    (if (equal (coerce row 'list) (coerce is 'list))
        (return-from in_table t))
    )
  )

(defun distinct (table)
  "create new table with unique values"
  (let ((result (simple-table:make-table)))
    (simple-table:with-rows (table row)
      (if (not (in_table result row))
          (simple-table:add-to-table row result))
      )
    (return-from distinct result)
    )
  )
