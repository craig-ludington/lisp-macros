;; debugging-log.lisp

;; Demonstrate a macro that only evaluates its arguments when desirable.

(in-package :cl-user)

(let ((debug-features (copy-list '((:debug . nil) (:info . nil) (:warn . nil) (:error . nil)))))

  (defun want-debugging (feature)
    (cdr (assoc feature debug-features)))

  (defun set-debugging (feature) 
    (setf (cdr (assoc feature debug-features)) 
          t))

  (defun clear-debugging (feature) 
    (setf (cdr (assoc feature debug-features)) 
          nil))

  ;; Function arguments are evaluated before the function is called.
  ;;
  ;;(defun debug-log (feature format-string &rest args) 
  ;;     (when (want-debugging feature)
  ;;       (apply #'format *error-output* format-string args)))
  ;; 
  ;; (clear-debugging :debug) 
  ;; (debug-log :debug "Long, expensive calculation: ~D~%" (long-expensive-calculation))

  ;; Macros can write code that doesn't necessarily evaluate the macro arguments.

  (defmacro debug-log (feature format-string &rest args)
    `(when (want-debugging ,feature)
       (format *ERROR-OUTPUT* ,format-string ,@args))))
