;; create structure for table
(defstruct table
  name
  rows_names
  rows
  )



(defun create_table (filename  rows)
  (make-table
   :name (car (split filename #\.))
   :rows_names (coerce (simple-table:get-row 0 rows) 'list)
   :rows rows
   )
  )

(defun read_table (filename)
  (cond
    ( (string-include ".csv" filename) (create_table
                                        filename
                                        (simple-table:read-csv filename t)))
    ( (string-include ".tsv" filename) (create_table
                                        filename
                                        (simple-table:read-tsv filename t)))
    (t (create_table
        filename
        (simple-table:make-table)))
    )

  )
