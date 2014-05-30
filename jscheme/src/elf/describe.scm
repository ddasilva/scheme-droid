{
Example of using import and define-method.  
(describe x) uses JDK 1.2 AccessibleObject to describe private
fields of object x.  

Example:
> (describe '(1 2 3))
an instance of jsint.Pair
  first: 1
  rest: (2 3)
()

(describe) calls the generic function (describe-object) which returns
a string.

Describing a class will show it constructors, fields, and methods.
Describing a procedure or generic function will display its definition.

(whoCalls symbol) returns a list of the names of the procedures
that call the procedure named symbol.
Example:
> (whoCalls 'short-toString)
(describe-object)
}

(import "java.io.PrintWriter")
(import "java.io.StringWriter")
(import "java.lang.Class")
(import "java.lang.System")
(import "java.lang.reflect.AccessibleObject")
(import "java.lang.reflect.Array")
(import "java.lang.reflect.Constructor")
(import "java.lang.reflect.Field")
(import "java.lang.reflect.Method")
(import "java.lang.reflect.Modifier")
(import "java.util.Hashtable")
(import "jsint.U")

(use-module "elf/util.scm" 'import 'all) ; (java-version>=1_2 isNull)
(use-module "elf/sort.scm" 'import 'all) ; (sort by)
(use-module "elf/iterate.scm" 'import 'all) ; (for-each* map* separate)
(use-module "elf/eopl2/jscheme/genwrite.scm" 'import 'all) ; (pretty-print)

(define (maps f xs) (apply string-append (map* f xs)))

(define-macro (ifs condition string) `(if ,condition ,string ""))

;;; printingToString: (PrintWriter -> void) -> String
(define (printingToString f)
  (let ((s (StringWriter.)))
    (f (PrintWriter. s))
    (.toString s)))

(define (ppString string)
  ;; Read an sexpression from string and pretty print it back to a string.
  ;; If any error occurs just return the string.
  (tryCatch
   (printingToString (lambda (s) (pp (string->expr string) s)))
   (lambda (e)
     (print `(ppString ,e))
     string)))
  
(define (ppToString exp) (printingToString (lambda (s) (pp exp s))))

(define make-accessible
  ;; Make Field vector accessible, punting if not in JDK 1.2
  (if (java-version>=1_2)
      (lambda (fs) (AccessibleObject.setAccessible fs #t) fs)
      (lambda (fs) fs)))

;;; (describe) requires JDK 1.2 to get access to all fields.
(if (java-version>=1_2)
    (import "java.lang.reflect.AccessibleObject"))

(define (memoize-1 f size)
  ;; Memoize (f x) using a Hashtable of size size.
  (let ((table (Hashtable. size)))
    (lambda (key)
      (let ((it (.get table key)))
	(if (not (isNull it)) it
	    (let ((it (f key)))
	      (.put table key it)
	      it))))))

(define all-fields
  ;; All fields, including private ones, most specific first.
  (memoize-1
   (lambda (c)
     (define (all-fields-1 super so-far)
       (if (isNull super) 
	   (apply append 
		  (map (lambda (fs) (make-accessible fs) (vector->list fs))
		       so-far))
	   (all-fields-1 (.getSuperclass super) 
			 (cons (.getDeclaredFields super) so-far))))
     (all-fields-1 c '()))
   100))

(define all-methods
  (memoize-1
   (lambda (c)
     (define (all-methods-1 super so-far)
       (if (isNull super)
	   (apply append
		  (map (lambda (fs) (make-accessible fs)
			       (vector->list fs))
		       so-far))
	   (all-methods-1 (.getSuperclass super)
			  (cons (.getDeclaredMethods super) so-far))))
     (all-methods-1 c '()))
   100))

(define (short-toString x max)
  (tryCatch
   (let ((it (.toString x)))
     (if (<= (string-length it) max) it
	 (string-append (substring it 0 (- max 4)) " ...")))
   (lambda (e) {Unprintable-[(.getName (.getClass x))]})))

(define (describe x . s)
  (let ((s (or (and (pair? s) (car s)) (current-output-port))))
    (if (eq? x #null) (display {[x] is null\n} s )
	(begin
	  (display (describe-object x) s)
	  (newline)))))

(define-method (describe-object (x java.lang.Object))
  {[(short-toString x 80)]
 is an instance of [(.getName (.getClass x))]
[(describe-fields x (.getClass x))]})

(define (wrap-last-class d)
  (let ((last-class #null))
    (lambda (m)
      (let ((result 
	     (if (not (eqv? last-class (.getDeclaringClass m)))
		 {\n  // from [(.getName (.getDeclaringClass m))]\n[(d m)]}
		 (d m))))
	(set! last-class (.getDeclaringClass m))
	result))))

(define (describe-fields x class)
  (if (java-version>=1_2)
   ;; Describe every field.
      (let ((fs (.getDeclaredFields class)))
       (AccessibleObject.setAccessible fs #t) ; Make them all accessible.
       (let ((thisClass (maps
			 (wrap-last-class
			  (lambda (f)	; Not static fields.
			    (if (not (Modifier.isStatic (.getModifiers f)))
				(describe-field f x)
				"")))
			 fs))
	     (others (let ((superclass (.getSuperclass class)))
		       (if (not (isNull superclass))
			   (describe-fields x superclass)
			   ""))))
	 {[others][thisClass]}))
      ;; Describe only public fields.
      (let ((fs (.getFields class)))
     (maps
      (wrap-last-class
       (lambda (f)			; Not static fields.
	 (if (not (Modifier.isStatic (.getModifiers f)))
	     (describe-field f x)
	     "")))
      fs))))

(define (describe-field f x)
  {  [(.getName f)]: [(U.stringify (.get f x) #t)]\n})

(define (dshow . items)
  {[(apply displays items)]\n})

(define-method (describe-object (x java.lang.Class)) (describe-class x #t))

(define (class-cpl c)
  ;; Return a list describing the class precedence list of class c.
  ;; > (class-cpl (class "java.lang.Class"))
  ;; (class java.lang.Class class java.lang.Object
  ;;   interface java.io.Serializable)
  (class-cpl-1 '() (list c)))

(define (class-cpl-1 so-far tail)
  (if (null? tail) (reverse so-far)
      (let* ((c (car tail))
	     (tail (cdr tail))
	     (is (vector->list (.getInterfaces c)))
	     (super (.getSuperclass c)))
	(class-cpl-1 (cons c so-far) 
		     (if (not (isNull super))
			 (cons super (append is tail))
			 (append is tail))))))

(define (displays . items) 
  (maps (lambda (i) (if (pair? i) (apply displays i)
			i))
	items))

(define (describe-class the-class all?)
  ;; Describe class c trying to use every method of the class Class.
  ;; if an all? argument is provided, show all public methods and 
  ;; fields, otherwise show the declared ones.
  (define (describe-items name what)
    (ifs (and (not (isNull what)) (not (= (vector-length what) 0)))
	{[name]: [(maps indent-print what)]\n}))
  (define (indent-print what) {  [(U.stringify what #t)]\n})
  (define (describe-item name what)
    (ifs (and what (not (isNull what))) {[name]: [(U.stringify what #t)]\n}))
  (define (class-or-error the-class) (U.toClass the-class))
  (let ((c (class-or-error the-class)))
    (string-append
     {class: [c]\n}
     (ifs (.isPrimitive c) "primitive ")
     (Modifier.toString (.getModifiers c))
     (if (.isInterface c) " " " class ")
     (.getName c)
     (let ((super (.getSuperclass c)))
       (ifs (not (isNull super)) { extends [(.getName super)]}))
     (let ((interfaces (.getInterfaces c)))
       (ifs (and (not (isNull interfaces)) (> (vector-length interfaces) 0))
	    {\n  implements [(separate " "
				       (map* (lambda (n)
					       (short-class-name n))
					     interfaces))]}))
    "\n"
    (describe-item "HashCode" (.hashCode c))
    (describe-item "ClassLoader" (.getClassLoader c))
    (if (java-version>=1_2) (describe-item "Package" (.getPackage c)))
    (describe-item "Name" (.getName c))
    (describe-item "isArray" (.isArray c))
    (describe-item "ComponentType" (.getComponentType c))
    (describe-item "DeclaringClass" (.getDeclaringClass c))
;    (if (java-version>=1_2)
;	 (describe-item "ProtectionDomain" (.getProtectionDomain c)))
    (describe-items "Signers" (.getSigners c))
    "\n// Constructors\n"
    (maps display-constructor (.getDeclaredConstructors c))
    "\n// Fields"
    (maps (wrap-last-class display-field)
	  ((if all? all-fields .getDeclaredFields)
	   c))
    "\n// Methods"
    (maps (wrap-last-class display-method)
	  ((if all? all-methods .getDeclaredMethods) c))
       ;; KRA 13JAN99: Causes access violoation on NT and W95.
    (in-1_2 #t
	    (describe-items "\n// Classes" (.getDeclaredClasses c)))
    )))

(define (modifier-string m) (Modifier.toString (.getModifiers m)))

(define (static-final? m)
  (let ((ms (.getModifiers m)))
    (and (Modifier.isStatic ms) (Modifier.isFinal ms))))

(define (class-name-name name)
  (let ((i (.lastIndexOf name ".")))
    (if (= i -1) name
	(.substring name (+ i 1)))))

(define (short-class-name c)
  (if (.isArray c)
      (string-append (short-class-name (.getComponentType c)) "[]")
      (class-name-name (.getName c))))

(define (commacomma items)
  (append (list "(") (separate ", " items) (list ")")))

(define (display-constructor m)
  (displays "  " (modifier-string m)
	    " " (short-class-name (.getDeclaringClass m))
	    (commacomma
	     (map* short-class-name (.getParameterTypes m)))
	    "\n"))

(define (display-field f)
  {  [(modifier-string f)] [(short-class-name (.getType f))] [(.getName f)]\n})

(define (display-method m)
  (dshow "  " (modifier-string m) " "
	    (short-class-name (.getReturnType m)) " "
	    (.getName m)
	    (commacomma
	     (map* short-class-name (.getParameterTypes m)))))

;;; 
;;; Describing procedures and generics.
;;; 

(define (applicationProcedure v)
  (vector-ref v 0))

(define (closureApplication? v)
  (and (vector? v)
       (> (vector-length v) 0)
       (eq? Closure.class (.getClass (applicationProcedure v)))))

(define (zip-var/vals vars vals)
  (cond ((null? vars) '())
	((pair? vars) (cons (list (car vars) (car vals))
			    (zip-var/vals (cdr vars) (cdr vals))))
	(else (list (list vars `(list ,@vals))))))

(define (backtraceBody lexenv args)
  ;; Used in BacktraceException.
  ;; (pp (revertLexenv lexenv (revertBody args))))
  (pp (revertBody args)))

(define (backtraceValueString c)
  ;; Used in LexicalEnvironment.
  (cond ((isNull c) "#null")
	;; Prints only (lambda (...) ...) not its enclosing environment.
	((instanceof c Closure.class) (ppToString (revertBody c)))
	(else (U.stringify c))))

(define (revertBody b)
  (if (isNull b) #null
      (revertBody0 b)))

(define-method (revertBody0 (b Closure))
  `(lambda ,(.parms$# b) ,(revertBody (.body$# b))))

(define-method (revertBody0 (v DynamicVariable)) (.name$# v))

(define-method (revertBody0 (v LocalVariable)) (.name$# v))

(define-method (revertBody0 (b Object)) b)
  
(define-method (revertBody0 (body Object[]))
  (define (maybeLet*? v)
    (and (closureApplication? v)
	 (= (length (.parms$# (applicationProcedure v))) 1)))
  (define (revertLet closure parameters)
    `(let ,(zip-var/vals (.parms$# closure) parameters)
       ,(revertBody (.body$# closure))))
  (define (revertLet* var/vals body)
    (if (maybeLet*? body)
	(let ((p (applicationProcedure body)))
	  (revertLet* (cons (list (car (.parms$# p))
				  (revertBody (vector-ref body 1)))
			    var/vals)
		      (.body$# p)))
	`(,(if (= (length var/vals) 1) 'let 'let*)
	  ,(reverse var/vals)
	  ,(revertBody body))))
  (cond ((maybeLet*? body)
	 (let ((p (applicationProcedure body)))
	   (revertLet* (list (list (car (.parms$# p))
				   (revertBody (vector-ref body 1))))
		       (.body$# p))))
	((closureApplication? body)
	 (let ((b (vector->list body)))
	   (revertLet (car b) (map revertBody (cdr b)))))
	(else (map* revertBody body))))
	
(define-method (revertBody0 (v Symbol)) v)

(define (revertLexenv e body)
  (if (eq? e LexicalEnvironment.NULLENV$) body
      (revertLexenv
       (.parent$# e)
       `(let ,(zip-var/vals (.vars$# e)
			    (map revertBody (vector->list (.vals$# e))))
	  ,body))))

(define (revertClosure f) (revertLexenv (.lexenv$# f) (revertBody f)))

(define-method (revert (f Closure))
  (revertClosure f))

(define-method (revert (g Generic))
  `(begin
     ,@(map
	(lambda (m)
	  `(method ,(map (lambda (name type) (list name (string->symbol
							 (.getName type))))
			 (.parms$# (cadr m))
			 (map* identity (car m)))
		   ,(revertClosure (cadr m))))
	(by 2 (vector->list (.methodTable$# g))))))

(define-method (describe-object (g Generic))
  {[g] is a generic function with methods:\n[(ppToString (revert g))]})

(define-method (describe-object (f Closure))
  {Closure named [(.getName f)]\n[(ppToString (revert f))]})

(define-method (describe-object (f Macro))
  {Macro named [(.getName f)]\n[(ppToString (revert f))]})

;;;
;;; Who calls
;;;

(define WHO_CALLS_TABLE #f)

;;; whoCalls: Symbol -> Symbol*
(define whoCalls
  (let ()
    (define (buildWhoCallsTable)
      (newline)
      (display "building who calls table ...")
      (let ((table (Hashtable. 2000)))
        (define (addWhoCalls c s)
          (let ((it (.get table c)))
            (.put table c 
                  (if (isNull it) (list s)
                      (if (member s it) it
                          (cons s it))))))
        ;; This does no analysis, just record every symbol in the body
        ;; of a procedure that has a global value.
        (iterate
         (.clone Symbol.symbolTable$)
         (lambda (s)
           (if (and (.isDefined s)
                    (or (instanceof (.getGlobalValue s) Closure.class)
                        (instanceof (.getGlobalValue s) Generic.class)))
               (for-each (lambda (call) (addWhoCalls call s))
                         (filter
                          (lambda (x)
                            (and (symbol? x) (.isDefined x)))
                          (flatten (revert
                                    (.getGlobalValue s))))))))
        (newline)
        table))
    (define (whoCallsTable)
      (if WHO_CALLS_TABLE WHO_CALLS_TABLE
          (begin (set! WHO_CALLS_TABLE (buildWhoCallsTable))
                 WHO_CALLS_TABLE)))
    (lambda (symbol)
      (let ((it (.get (whoCallsTable) symbol)))
        (if (not (isNull it))
            (sort it (comparator string<=? symbol->string))
            '())))))
