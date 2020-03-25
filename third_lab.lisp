

;; Third lab for LISP. Made by Gricaenko Ekatereana. IP-82


(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv" t))
(defvar mp_posts_full (simple-table:read-csv #P"mp-posts_full.csv" t))


(print "Start work with cli-server")
(terpri)

;;;; text formatting
(defun string-include (s1 s2)
  (cond
    ((zerop (length s1)) nil)
    ((> (length s2) (length s2)) nil)
    ((string= s1 (subseq s2 0 (length s1))) s1)
    (t (string-include s1 (subseq s2 1))) ))

(defun create_format_str (number str)
  (cond
    ((eq number 1) str)
    (t (create_format_str
        (- number 1)
        (concatenate 'string str " |~20@A|")))
    )
  )


(defun pretty_view (table)
  (simple-table:with-rows (table row)
    (print  (format t "~?"
                    (create_format_str
                     (simple-table:num-cols row) "|~6@D|")
                    (coerce row 'list)))
    )
  )


(defun execute_command (command)
  (cond
    ((string= (string-downcase command) "exit") (exit))
    ((string-include "load"
                     (string-downcase command))
                     (pretty_view map_zal ))
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
  






