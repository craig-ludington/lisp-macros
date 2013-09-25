;; form-bind.lisp

(in-package :cl-user)

(defvar *form-values* (make-hash-table :test #'equalp)
  "Bound to the current request's form values.")


(defun to-boolean (s)
  "Return a boolean value for a form value."
  (and s (or (string-equal s "True")
             (string-equal s "Yes"))))

#- (and)
;;;
;;; This is the code I'd *like* to write.
;;; 
(form-bind (;; Create a lexical binding for 'name with the value of the form field "name" 
            ;; as a string of zero or more characters (never NULL).
            name
            ;; Bind the form field "married" to 'married with a boolean value.
            (married to-boolean)
            ;; Bind "age" to 'years, as an integer, defaulting to 0
            ((age years) parse-integer 0))
  (cond ((zerop (length name))
         (chide "Name is required."))
        ((< years 21)
         (chide "This is an adults-only service."))
        (married
         (chide "Restrictions may apply."))
        (t
         (proceed))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun %form-bind (args)
    "Helper function for the form-bind macro."
    (let (result-forms)
      (do* ((arg        
             args
             (cdr arg))
            (spec
             #1=(car arg)
             #1#)
            (form-name  
             #2=(if (atom spec) 
                    spec
                    (let ((names (first spec)))
                      (if (atom names)
                          names
                          (first names))))
             #2#)
            (var-name
             #3=(if (atom spec)
                    spec
                    (let ((names (first spec)))
                      (if (atom names)
                          names
                          (second names))))
             #3#)
            (function
             #4=(or (and (consp spec) (second spec))
                    'identity)
             #4#)
            (default
                #5= (if (atom spec)
                        ""
                        (third spec))
              #5#))
           ((endp arg))
        (let* ((not-found (gensym "NOT-FOUND-"))
               (found (gensym "FOUND-")))
          (push `(,var-name (let ((,found (gethash (symbol-name ',form-name) *form-values* ',not-found)))
                              (if (eq ,found ',not-found)
                                  ,default
                                  (funcall #',function ,found))))
                result-forms)))
      (nreverse result-forms))))

(defmacro form-bind (binding-specs &body body)
  "Create a lexical binding using LET for each of BINDING-SPECS and execute BODY
inside that LET.

A binding-spec may be a symbol or a list of up to three elements.

If the binding-spec is a symbol, it is used as both the variable name and as a 
key for a lookup in the hash table *FORM-VALUES*.  Its value is always a string,
possibly of zero-length.  

For example, if binding-spec is the symbol NAME, the variable created would be 
'NAME, and its value would be the result of (GETHASH \"NAME\" *FORM-VALUES*).

If the binding-spec is a list, it specifies the form name to lookup, optionally,
a different variable name to use, an optional function of a single argument to transform
the value, and an optional default value to use if the lookup fails.

Here's a grammar:

BINDING-SPECS := BINDING-SPEC ...

BINDING-SPEC  := SYMBOL
              |  ( NAME-SPEC CONVERSION-FUNCTION DEFAULT-VALUE )

NAME-SPEC     := FORM-NAME
              |  ( FORM-NAME VARIABLE-NAME )

Here's an example:

    (form-bind (x 
                (y parse-integer)
                (z parse-integer 99)
                ((n zee-prime))"
  `(let (,@(%form-bind binding-specs))
     ,@body))

(defun test-form-bind ()

  (clrhash *form-values*)
  (setf (gethash "name" *form-values*) "Bob")
  (setf (gethash "married" *form-values*) "True")
  (setf (gethash "age" *form-values*) "22")
  (form-bind (name
              (married to-boolean)
              ((age years) parse-integer 0))
    (assert (equalp '("Bob" T 22) (list name married years))))

  (clrhash *form-values*)
  (setf (gethash "name" *form-values*) "Bob")
  (setf (gethash "married" *form-values*) "no")
  (form-bind (name
              (married to-boolean)
              ((age years) parse-integer 0))
    (assert (equalp '("Bob" NIL 0) (list name married years))))

  (clrhash *form-values*)
  (form-bind (name
              (married to-boolean T)
              ((age years) parse-integer 0))
    (assert (equalp '("" T 0) (list name married years)))))




