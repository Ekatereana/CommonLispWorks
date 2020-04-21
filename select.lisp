;; function select
(load "distinct.lisp")
(load "where.lisp")
(load "query-builder.lisp")

(defstruct select-statement
  columns
  is_distinct
  sourse
  condition
  and
  or
  order-by
  limit
  )

(defun read-select (query)
  "read parts of select statement"
  (let ((clean_list (clean-list (split query))))

    (make-select-statement
     :columns (get_args clean_list "select" "from")
     :is_distinct (string-include "distinct" query )
     :sourse (get_args clean_list "from" "where")
     :condition (get_args clean_list "where" "limit")
     :and (string-include "and" query)
     :or (string-include "or" query)
     :order-by (string-include "order by" query)
     :limit (get_args clean_list "limit")
     
     )  

    )
 
  )



  (defun select (statement)
    (let (
          (table)
          (query (read-select statement ))
          )
      ;; identify table that we use.    
      (setq table (gethash (car (select-statement-sourse query)) datasourse))
      (cond
        ((null table) (print "ERROR::: Unknown table, please try again"))
        ((string-include statement  "*")
         (pretty_view table))
        (t (pretty_view
            (make_query_table
             table
             (select-statement-columns query)
             (select-statement-is_distinct query)
             (select-statement-condition query)
             (select-statement-and query)
             (select-statement-or query))))
        )
      ))


