;; Made by Gricaenko Ekatereana.


;; add data-sources files to hash-table
(defvar datasourse (make-hash-table :test 'equal))
(setf (gethash "map_zal-skl9" datasourse) (read_table "data-sources/map_zal-skl9.csv"))
(setf (gethash "mp_posts_full" datasourse) (read_table "data-sources/mp-posts_full.csv"))
(setf (gethash "test" datasourse) (read_table "data-sources/test.csv"))

;;import basic custom functions
(load "formatter.lisp")
(load "reader.lisp")
(load "select.lisp")


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
    
    ((string-include "select" (string-downcase command))
     (select (string-downcase command)))
    
    ((string-include "load" (string-downcase command))
     (load_table (cdr (split command))))
    
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
;(print (select "select col, pos_y, title from test where col > 21 and col < 24"))
;(print (select "select col, pos_y, title from test"))
(print (select "select col, pos_y, title from test order by col asc"))
(print (select "select col, pos_y, title from test order by col desc"))
(print (select "select min ( col ), title from test"))
(print (select "select col, pos_y, title from test order by col desc"))
(print (select "select avg ( col ) from test"))




