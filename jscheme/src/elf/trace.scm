;;;
;;; Very simple trace facility.
;;;

;;; Usage:
;;; (trace 'functionName) - trace function named functionName.
;;; (untrace 'functionName) - stop tracing.
;;; (untrace) - stop all tracing

(use-module "elf/iterate.scm" 'import 'all)

(define *traced-table* (Hashtable. 10))

(define (trace name)
  (if (and (symbol? name) (.isDefined name))
      (if (member name '(.get
			 .put
			 .setGlobalValue
			 apply
			 display
			 error
			 string-append
			 trace
			 untrace
			 trace-wrap
			 ))
	  (error "Can't trace " name "! It is used by trace itself.")
	  (let ((f (eval name)))
	    (.put *traced-table* name f)
	    (.setGlobalValue name (trace-wrap name f))))
      (error name " must be a defined symbol")))
			   
(define (untrace . name)
  (if (pair? name)
      (let ((name (car name)))
	(let ((f (.get *traced-table* name)))
	  (if (not (eq? f #null))
	      (.setGlobalValue name f))))
      (for-each* (lambda (e)
		   (.setGlobalValue (.getKey e) (.getValue e)))
		 (.entrySet *traced-table*))))

(define (trace-wrap name f)
  (define (trace-enter name args)
    (newline) (display "Enter: ")
    (write `(,name ,@args)) (newline))
  (define (trace-return name value)
    (newline) (display "Return; ") (display name) (display " ")
    (write value) (newline))
  (lambda args
    (trace-enter name args)
    (let ((value (apply f args)))
      (trace-return name value)
      value)))
