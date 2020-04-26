
(defstruct select-statement
  function
  args_of_func
  columns
  is_distinct
  sourse
  inner-join
  full-outer-join
  left-join
  right-join
  condition
  and
  or
  order-by
  order-way
  limit
  )


(defvar key-words-query)
(setq key-words-query '( "(" ")"  "select" "from" "inner join on" "full outer join on"
                        "left join on"
                        "right join on" "where" "order by" "limit"))

(defvar function-words-query)
(setq function-words-query '( "min" "avg"))

;; function select
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "functions.lisp")
(load "query-builder.lisp")
(load "innerjoin.lisp")


(defun is_second (probably query)
  (let ((iterator (position  probably key-words-query :test #'string=)))
    (loop while (< iterator (length key-words-query))
          do (if (string-include (nth (+ 1 iterator) key-words-query) query)
                 (progn  ( setq probably (nth (+ 1 iterator) key-words-query))
                         (return-from is_second probably))
                
                 )
             (setq iterator (+ 1 iterator))
            
          )
    )
  )

(defun get_order_way (statement)
  "if statement include asc or desc order to sort table"
  (cond
    ((string-include "asc" statement) "asc")
    ((string-include "desc" statement) "desc")
    (t nil))
  )

(defun get_function_name (qeury)
  (cond
    ((string-include "min" qeury) "min")
    ((string-include "avg" qeury) "avg")
    (t nil))
  )

(defun read-select (query)
  "read parts of select statement"
  (let ((clean_list (clean-list (split query))))

    (make-select-statement
     :function (get_function_name query)
     :args_of_func (get_args clean_list "(" ")")
     :columns  (delete "distinct" (get_args clean_list (is_second "(" query) "from"))
     :is_distinct (string-include "distinct" query )
     :sourse (get_args clean_list "from" (is_second "from" query))
     :inner-join (get_args clean_list "inner join on" (is_second "inner join on" query))
     :condition (get_args clean_list "where" (is_second "where" query))
     :and (string-include "and" query)
     :or (string-include "or" query)
     :order-way (get_order_way query)
     :order-by (get_args clean_list "order" (get_order_way query))
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
     ;; (print query)
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
             (select-statement-order-way query)
             (select-statement-function query)
             (select-statement-args_of_func query)
             (select-statement-inner-join query))))
        )
      ))


