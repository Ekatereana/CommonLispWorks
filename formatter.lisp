(require 'asdf)

(load "cl-simple-table/cl-simple-table.asd")

(asdf:load-system 'cl-simple-table)

;;;; text formatting
(defun string-include (s1 s2)
  (cond
    ((zerop (length s1)) nil)
    ((> (length s1) (length s2)) nil)
    ((string= s1 (subseq s2 0 (length s1))) s1)
    (t (string-include s1 (subseq s2 1)))))

(defun create_format_str (number str)
  (cond
    ((eq number 1) str)
    (t (create_format_str
        (- number 1)
        (concatenate 'string str " |~20@A|")))
    )
  )

(defun split (string &optional (split-character #\Space))
  (let ((result '())
        (stream (make-string-output-stream)))
    (loop for char across string
          if (char= char split-character)
          do (push (get-output-stream-string stream) result)
          else
          do (write-char char stream))
    (push (get-output-stream-string stream) result)
    (nreverse result)))

(defun clean-list (list &optional (output '()))
  "remove spaces and comas from list"
  (cond
    ((null list) output)
    ((eq (string= (car list) "") nil)
     (clean-list (cdr list)
                 (append output  (list (remove #\, (car list))))))
    (t (clean-list (cdr list) output))
    )
  )



(defun get_args (list key &optional  (stop) (on) (output '()))
  "get arg`s of query statement"
  (cond
    ((string= key  (car list))
                   (get_args (cdr list) key stop (setq on t)))
    ( (or (null list) (string= stop (car list))) output)
    ((eq on t) (get_args (cdr list) key stop on
                         (append output (list (car list)))))
    (t
     (get_args (cdr list)
               key stop))
    )
  )

(defun pretty_view (table)
  "print table with pretty view"
  (let ((cl_table (table-rows table)))
    "iterator over table rows "
    (print (format nil "~?"
                   (create_format_str  (length (table-rows_names table)) "|~8@A|")
                   (table-rows_names table)))
    (print
     (format nil "~v@{~3@A~:*~}" (* (length (table-rows_names table)) 6) "_ "))
    (simple-table:with-rows (cl_table row)
      (print  (format nil "~?"
                      (create_format_str  (simple-table:num-cols row) "|~8@A|")
                      (coerce row 'list)))
      )
    )
  
  )
