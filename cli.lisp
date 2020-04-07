;; Third lab for LISP. Made by Gricaenko Ekatereana. IP-82


(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

;;import basic custom functions
(load "formatter.lisp")
(load "reader.lisp")

;; add data-sources files to hash-table
(defvar datasourse (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" datasourse) (read_table "data-sources/map_zal-skl9.csv"))
(setf (gethash "mp_posts_full" datasourse) (read_table "data-sources/mp-posts_full.csv"))


;; function select

(defstruct select-statement-q
  columns
  sourse
  condition
  limit
  )

(defun read-select (clean_list)
  "read parts of select statement"
  (make-select-statement-q
   :columns (get_args clean_list "select" "from")
   :sourse (get_args clean_list "from" "where")
   :condition (get_args clean_list "where" "limit")
   :limit (get_args clean_list "limit")
   )  
  )

(defun select (statement)
  (read-select (clean-list (split statement)))
  )

(defun pretty_view (table)
  "print table with pretty view"
  (let ((cl_table (table-rows table)))
    "iterator over table rows "
    (simple-table:with-rows (cl_table row)
      (print  (format t "~?"
                      (create_format_str
                       (simple-table:num-cols row) "|~6@D|")
                      (coerce row 'list)))
      )
    )
 
  )

;;function load-table
(defun load_table (name)
  (cond
    ((eq (gethash (car name) datasourse) nil) (print "Unknown table, try again."))
    (t (pretty_view (gethash (car name) datasourse)))
    )
  )



(defun execute_command (command)
  "process line that user enters"
  (cond
    ((string= (string-downcase command) "exit") (exit))
    ((string-include "load"
                     (string-downcase command))
       (load_table (cdr (split command)))
     )
    (t (princ command))
    )
  )


(defun run ()
  "run cli"
  (terpri)
  (princ " [cl-user@host ~]$: ")
  (execute_command (read-line)) 
  (run)
  )


;; start Server
(print "Start work with cli-server")
;;(print "Command should ends with ;")
(terpri)
;;(run)

;;(execute_command (read-line))
(print (select "select t from j"))
