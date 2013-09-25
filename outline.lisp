


(defun talk (&optional (stream *standard-output*))
  (cl-who:with-html-output (stream)
    (:html
     (:head :title "Macros")
     (:body
      (:h1 "Macros")

      (:h2 "Introduction")
      (:p "Macros are programs that write programs.  As Paul Graham writes in On Lisp,")
      (:p ((:blockquote :attribution "Paul Graham") 
           "Lisp's macro facility allows you to define operators that are implemented by transformation. The definition of a macro is essentially a function that generates Lisp code &mdash; a program that writes programs."))

      (:h2 "Transforming Lisp Code into Lisp Code")
      (:h3 "A Quick Example")
      (:h4 "DEFMACRO &mdash; define a macro")
      (:pre "  (defmacro nullify (arg)
    (list 'setq arg 'nil))")
      (:h4 "Invoke a macro")
      (:pre "  (nullify *x*)")
      (:h4 "The compiler sees:")
      (:pre "  (setq *x* 'nil)")
      (:h3 "MACROEXPAND-1 &mdash \"Preview\" for Macros")
      (:p "MACROEXPAND-1 is a function that expands a macro invocation.  At the REPL, call it with the quoted source form:")
      (:pre "  (macroexpand-1 '(nullify *x*))")
      (:p "With SLIME you can put your cursor on the opening parenthesis of the form and press C-c Enter.  That's the easy way to macroexpand.  Either way, you'll see a preview of the code the compiler will process.")

      (:h2 "Differences between a function and a macro")
      (:h3 "Invoking a macro and invoking a function have the same syntax.")
      (:p "We could define a function or a macro called SHOW-STRING:") 
      (:pre "
  (defun show-string (s) 
    (print s))
  (defmacro show-string (s) 
    (list 'print s))")
      (:p "We'd invoke either one the same way:")
      (:pre "  (show-string \"hello, world\") ;; Macro or function invocation?")
      (:h3 "Functions are first-class objects.") 
      (:h4 "May be passed as arguments to another function.")
      (:pre "  (mapcar '+ '(1 2 3))") 
      (:h4 "May be stored in a variable.") 

      (:h3 "Macros are instructions for the parser.")
      (:h4 "May not be passed as arguments or stored in a variable.")
      (:h4 "Disappear after parsing -- replaced by the emitted code.")
      (:pre "
  (let ((function '+))
    (mapcar function '(1 2 3)))
  (defmacro my-plus (&rest args)
    (list 'apply '+ args))
  (mapcar 'my-plus '(1 2 3)) => ERROR -- MY-PLUS isn't a function.")
      (:h3 "Arguments are evaluated differently.")
      (:h4 "Function arguments are evaluated before the function is called.")
      (:pre "  (+ 1 (* 2 3))")
      (:p "The function '+ receives two arguments, 1 and 6.")
      (:h4 "Macro arguments are passed to the macro unevaluated.")
      (:pre "  (my-plus 1 (* 2 3))")
      (:p "The macro MY-PLUS receives two arguments, 1 and the list (* 2 3).")

      (:h2 "Lisp source code isn't text -- it's a parse tree of Lisp objects.")
      (:p "The code that you write is an external representation of the parse tree. The code that the compiler operates on is an internal representation,made up of Lisp objects, not text.")
      (:h3 "The parse tree can be built by reading text that you type.")
      (:pre "  (print \"hello, world\")")
      (:h3 "The parse tree can just as well be built by calling a function that you write.")
      (:pre "  (list 'print \"hello, world\")")
      (:h3 "The compiler doesn't distinguish between the origins of the parse tree.")


      (:h2 "Macroexpansion \"time\" vs. Evaluation \"time\"")
      ((:blockquote :attribution "Paul Graham") 
       "The macroexpansion step deals with expressions, and the evaluation step deals with their values.")
      (:h3 "Macroexpansion happens first, while the code is being read.")
      (:h4 "A macro invocation is replaced by the Lisp that it produces.")
      (:h4 "Macroexpansion deals with source code, not program data.")
      (:pre "
  (defmacro add-one (number) 
    (if (eql number 13)
      (list 'error \"bad luck!\")
      (list '1+ number)))
  
  (add-one 12)
  (add-one 13)     ;; Emits the error form
  (let (num 13)
    (add-one num)) ;; Emits the 1+ form")

      (:h3 "Evaluation happens later, maybe much later.")
      (:h4 "A macro invoked at the REPL is evaluated immediately after macroexpansion")
      (:h4 "A macro invoked inside a function definition is evaluated when the function is called")


      (:h2 "Making Lists with Backquote")
      (:p "The backquote operator builds lists, just like the functions LIST and APPEND do, or much like the QUOTE special operator does.  Backquote may be more convenient to use, since its input looks a lot like its output does.  Using backquote is like filling in a template to make a list.")
      
      (:p "Backquote makes macros easier to write and read.")
      
      (:h3 "Without interpolation, backquote works just like quote")
      
      (:p "These all produce the same list:")
      
      (:pre "
  `(a b c) 
  '(a b c) 
  (list 'a 'b 'c)")
      
      (:h3 "Quoting with interpolation")
      (:h4 "Comma interpolates")
      (:p "The comma operator \"unquotes\" the next form, the value of which is inserted into the list at that point.")
      
      (:pre "
  (let ((a 1) (b 2))
    `(a ,b)) 
  Result: (A 2)")
      
      (:h4 "Comma At-sign splices")
      (:p "The comma at-sign operator also takes the value of the next form, which must be a list (unless it occurs last) and splices its values into the result.")
      (:pre "
  (let ((list '(one two)))
    `(,list ,@list))
  Result: ((ONE TWO) ONE TWO)")

      (:h2 "Variable capture")
      (:h3 "The problem with variable capture.")
      (:p "Here we construct a new flow-control FOR loop. We want to avoid evaluating STOP multiple times because it may be a form with side-effects. We create a local variable LIMIT, with the value of STOP. But the variable LIMIT is captured.")

      (:pre "
  (defmacro for ((var start stop) &body body)
    `(do ((,var ,start (1+ ,var))
          (limit ,stop))
         ((> ,var limit))
       ,@body))")

      (:p "If the programmer writes")
      (:pre "
  (for (x 1 10)
    (print x))")
      (:p "which macroexpands into")
      (:pre "
  (DO ((X 1 (1+ X))
       (LIMIT 10))
      ((> X LIMIT))
    (PRINT X))")
      (:p "the variable capture isn't a problem.")

      (:p "The problem occurs when the programmer happens to use the same variable name in the body of the loop.")
  
      (:pre "
  (let ((limit 42))
    (for (x 1 10)
      (format t \"~D ~D~%\" x limit)
      (decf limit (* 10 x))))")
      (:p "The LIMIT that's printed and decremented are the variable created in the DO, not in the LET as the programmer expects.")


      (:h3 "GENSYM")
      (:h4 "GENSYM creates a new symbol with a guaranteed unique identity.")
      (:h4 "We can use that new symbol safely in emitted code with no collisions possible.")

      (:p "Here, LIMIT is created, during macroexpansion, outside of the emitted code. It's never used in the emitted code.  What's used instead is the result of the call to GENSYM -- probably something like #:G2343")
      (:pre "
  (defmacro for ((var start stop) &body body)
    (let ((limit (gensym \"LIMIT-\")))
      `(do ((,var ,start (1+ ,var))
            (,limit ,stop))
           ((> ,var ,limit))
         ,@body)))

  (let ((limit 42))
    (for (x 1 10)
      (decf limit))) ;; No surprises")

      
      )))
  
  (values))


#-(and)
(with-open-file (stream "/home/craigl/macros/outline.html" :direction :output :if-exists :supersede)
  (talk stream))
