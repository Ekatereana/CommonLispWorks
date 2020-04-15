
;; create structure for table
(defstruct table
  name
  rows_names
  rows
  )



(defun create_table ( rows &optional (filename "")
                                     (row_name (simple-table:get-row 0 rows)))
  (make-table
   :name (car (split filename #\.))
   :rows_names (coerce row_name 'list)
   :rows (delete row_name  rows)
   )
  )

(defun read_table (filename)
  (cond
    ( (string-include ".csv" filename) (create_table                                 
                                        (simple-table:read-csv filename t)
                                        filename))
    (() (string-include ".tsv" filename) (create_table                               
                                          (simple-table:read-tsv filename t)
                                          filename))
    (t (create_table
        (simple-table:make-table)
        filename))
    )

  )
