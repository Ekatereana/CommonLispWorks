;; function select
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "query-builder.lisp")


(defstruct select-statement
  columns
  is_distinct
  sourse
  condition
  and
  or
  order-by
  order-way
  limit
  )



(defvar key-words-query)
(setq key-words-query '( "from" "where" "order" "limit"))

(defun is_second (probably query)
  (let ((iterator (position  probably key-words-query :test #'string=)))
    (loop while (< iterator (length key-words-query))
          do (if (string-include (nth (+ 1 iterator) key-words-query) query)
                 (setq probably (nth (+ 1 iterator) key-words-query))
                 )
             (setq iterator (+ 1 iterator))
            
          )
    (return-from is_second probably)
    )
  )

(defun get_order_way (statement)
  "if statement include asc or desc order to sort table"
  (cond
    ((string-include "asc" statement) "asc")
    ((string-include "desc" statement) "desc")
    (t nil))
  )


(defun read-select (query)
  "read parts of select statement"
  (let ((clean_list (clean-list (split query))))
    (make-select-statement
     :columns (delete "distinct" (get_args clean_list "select" "from"))
     :is_distinct (string-include "distinct" query )
     :sourse (get_args clean_list "from" (is_second "from" query))
     :condition (get_args clean_list "where" (is_second "where" query))
     :and (string-include "and" query)
     :or (string-include "or" query)
     :order-way (get_order_way query)
     :order-by (delete "by" (get_args clean_list "order" (get_order_way query)))
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
             (select-statement-or query)
             (select-statement-order-by query)
             (select-statement-order-way query))))
        )
      ))


