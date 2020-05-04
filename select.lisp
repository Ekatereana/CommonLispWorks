
(defstruct select-statement
  function
  args_of_func
  columns
  is_distinct
  case
  case-name
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
  group-by
  having
  limit
  )


(defvar key-words-query)
(setq key-words-query '( "(" ")"
                        "select"
                        "case"
                        "end"
                        "as"
                        "from"
                        "inner join"
                        "full outer join"
                        "left join"
                        "right join"
                        "where"
                        "order by"
                        "group by"
                        "having"
                        "limit"))

(defvar function-words-query)
(setq function-words-query '( "min" "avg"))

;; function select
(load "distinct.lisp")
(load "where.lisp")
(load "orderby.lisp")
(load "functions.lisp")
(load "query-builder.lisp")
(load "innerjoin.lisp")
(load "fullouterjoin.lisp")
(load "sidejoin.lisp")
(load "groupby.lisp")
(load "having.lisp")
(load "case.lisp")


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
     :columns  (delete "distinct" (get_args clean_list (is_second "(" query) (is_second "select" query)))
     :is_distinct (string-include "distinct" query )
     :case (get_args clean_list "case" "end")
     :case-name (get_args clean_list "as" (is_second "as" query))
     :sourse (get_args clean_list "from" (is_second "from" query))
     :inner-join (delete "on" (get_args clean_list "inner join" (is_second "inner join" query))
                         :test #'string=)
     :full-outer-join (delete "on"
                              (get_args clean_list "full outer join" (is_second "full outer join" query))
                              :test #'string=)
     :right-join (delete "on" (get_args clean_list "right join" (is_second "right join" query)) :test #'string=)
     :left-join (delete "on" (get_args clean_list "left join" (is_second "left join" query)) :test #'string=)
     :condition (get_args clean_list "where" (is_second "where" query))
     :and (string-include "and" query)
     :or (string-include "or" query)
     :order-way (get_order_way query)
     :order-by (get_args clean_list "order by" (get_order_way query))
     :group-by (get_args clean_list "group by" (is_second "group by" query))
     :having (get_args clean_list "having" (is_second "having" query))
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
             (select-statement-group-by query)
             (select-statement-having query)
             (select-statement-function query)
             (select-statement-args_of_func query)
             (select-statement-inner-join query)
             (select-statement-full-outer-join query)
             (select-statement-right-join query)
             (select-statement-left-join query)
             (select-statement-case query)
             (select-statement-case-name query))))
        )
      ))


