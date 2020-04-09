(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

;; create structure for table
(defstruct table
  name
  rows_names
  rows
  )



(defun create_table ( rows &optional (filename ""))
  (make-table
   :name (car (split filename #\.))
   :rows_names (coerce (simple-table:get-row 0 rows) 'list)
   :rows rows
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
