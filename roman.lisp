;; Make a function to handle symbols that start with "#!"

(defun |#!-reader| (stream subchar arg)
  "Interpret the symbol following #! as a Roman numeral."
  (declare (ignore subchar arg))
  (list 'gethash 
        (symbol-name (read stream nil t nil))
        ;; The #. means "Evaluate this next form at READ time,
        ;; so the compiler sees an actual hash table with MMMCMXCIX
        ;; values already inserted.
        #.(let ((hash (make-hash-table :test 'equalp))) 
            (loop for x from 1 upto 3999 do
                 (setf (gethash (format nil "~@R" x) hash) x))
            hash)))

;; Tell the reader to use it
(set-dispatch-macro-character #\# #\! #'|#!-reader|)

;; Now do some arithmetic in Roman numerals:

(- #!MMMCMXCIX #!MMMCMXCVIII) ;; => 1

(loop for number from #!I upto #!V do (format t "#!~@R~%" number))
;; Output is read-able by the Lisp reader:
;; #!I
;; #!II
;; #!III
;; #!IV
;; #!V

(loop for number from #!I upto #!V do (print number))
;; The default printer still prints Arabic numbers.
;; 1 
;; 2 
;; 3 
;; 4 
;; 5 
