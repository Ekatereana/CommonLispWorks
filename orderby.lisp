(defun get_comparator (value)
  "returns function for comparing in asc order corresponds to value type"
  (cond
    ((numberp value) #'<)
    ((stringp value) #'string<)
    (t #'equal)
    )
  )

(defun get_comparator_reverse (value)
  "returns function for comparing in desc order corresponds to value type"
  (cond
    ((numberp value) #'>)
    ((stringp value) #'string>)
    (t  #'equal)
    )
  )

(defun get_equal (value)
  "returns function for checking if two values is equal"
  (cond
    ((numberp value) #'=)
    ((stringp value) #'string=)
    (t #'equal)
    )
  )


(defun create_comparator (order value)
  (cond
    ((string= "asc" order) (get_comparator value))
    ((string= "desc" order) (get_comparator_reverse value))
    (t (get_comparator value))
    )
  )

(defun compare_rows (comparator id row1 row2)
  (funcall comparator
           (simple-table:get-row-column id row1)
           (simple-table:get-row-column id row2))
    )


(defun get_compare (value id order)
  (lambda (row1 row2)
    (compare_rows (create_comparator order value) id row1 row2))
  
  )
  
  

(defun order_by (order id table-rows)
  "sort table with custom comparator"
  (sort table-rows (get_compare
                    (simple-table:get-row-column id (simple-table:get-row 0 table-rows))
                    id
                    order))
  )
