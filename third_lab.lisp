

;; Third lab for LISP. Made by Gricaenko Ekatereana. IP-82


(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv" t))
(defvar mp_posts_full (simple-table:read-csv #P"mp-posts_full.csv" t))
;;(defvar plentary_results_by_name (simple-table:read-csv #P"plentary_result_by_name-skl9.csv" t))
;(defvar plentary_register_mps (simple-table:read-tsv #P"plentary_register_mps-skl9.tsv"))


(print "Start work with cli-server")
(terpri)


(defun string-include (s1 s2)
  (cond
    ((zerop (length s1)) nil)
    ((> (length s2) (length s2)) nil)
    ((string= s1 (subseq s2 0 (length s1))) s1)
    (t (string-include s1 (subseq s2 1)))
                               ))

(defun pretty_view (table col_num)
  (simple-table:with-rows (table row)
    (print  (format nil "~?"  (concatenate 'string "|~6D|" (make-string col_num ?"|~8@A|") ) 
                    (coerce row 'list)))
    )
  )


(defun execute_command (command)
  (cond
    ((string= (string-downcase command) "exit") (exit))
    ((string-include "load"
                     (string-downcase command))
                     (pretty_view map_zal (simple-table:num-cols (simple-table:get-row (0 map_zal ))) ))
    (t (print command))
    )
)

(defun run ()
  "run cli"
  (terpri)
  (princ " [cl-user@host ~]$: ")
  (execute_command (read-line)) 
  (run)
  )
  
(run)
