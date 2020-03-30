

;; Third lab for LISP. Made by Gricaenko Ekatereana. IP-82


(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

(load "formatter.lisp")
(load "reader.lisp")


(defvar datasourse (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" datasourse) (read_table "map_zal-skl9.csv"))
(setf (gethash "mp_posts_full" datasourse) (read_table "mp-posts_full.csv"))

(defun load_table (name)
  (cond
    ((eq (gethash (car name) datasourse) nil) (print "Unknown table, try again."))
    (t (pretty_view (gethash (car name) datasourse)))
    )
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
(terpri)
;;(run)

(execute_command (read-line))


;(print (read_table "map_zal-skl9.csv"))
;;(setf (gethash "0" datasourse) '1)
;;(print (gethash "0" datasourse))
