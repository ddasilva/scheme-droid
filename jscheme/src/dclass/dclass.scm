(load "elf/basic.scm")
(load "elf/sort.scm")
(load "using/run.scm")
(load "dclass/emit.scm")

{
Known Bugs:
(emit (dclass-procedure 'foo '() '((Pattern.compile "^\\s*$")))
      (current-output-port))
produces
private static final Procedure foo =(Procedure) JS.eval("(lambda () (Pattern.compile \"^\\s*$\"))");
when it should be ^\\\\s*$.
}

;; Change the following variables:
;;; Root directory of where define-class generated *.java files
(define dclass-src-base (java.io.File. ($ "user.dir") "src"))
;;; Root directory of where compiled *.class files should go.
(define dclass-class-base dclass-src-base)

;;; Classpath to use by javac
(define dclass-classpath
  (apply string-append
	 (separate ($ "path.separator")
	    (map* .getFile (.getURLs (Import.getClassLoader))))))

"
Each dclass clause is parsed into one of these record classes:

Item
  Import
  Package
  Static
Modified
  Class
  Field
  Invokable
    Constructor
    Method

Class is also used as a container for all the clauses.

"
(define (filter-class c xs)
  (filter (lambda (x) (instanceof x c)) xs))

(define (filter-class/1 c xs) (first (filter-class c xs)))

(define (package class)
  (.item$ (filter-class/1 dclass.Package.class (.clauses$ class))))

(define (imports class)
  (map .item$ (filter-class dclass.Import.class (.clauses$ class))))

(define (members class)
  (filter (lambda (c) (or (instanceof c dclass.Static.class)
			  (instanceof c dclass.Field.class)
			  (instanceof c dclass.Invokable.class)))
	  (.clauses$ class)))

;;; Utilities

;;; (gensym name) -> generates a unique name.
;;; (gensym-reset -> resets the gensym counter.
(define gensym #f)
(define gensym-reset #f)
(let ((count 0))
  (set! gensym
	(lambda (name)
	  (let ((it (string->symbol (string-append name count))))
	    (set! count (+ count 1))
	    it)))
  (set! gensym-reset (lambda () (set! count 0))))

(define (last xs)
  (define (last0 xs)
    (if (null? (cdr xs)) xs
	(last0 (cdr xs))))
  (if (null? xs) '()
      (last0 xs)))

(define-macro (define-class . clauses)
  `(define-class-runtime ',clauses))

(define (define-class-runtime clauses)
  (define (class->file name)
    ;; (class->file "foo.bar.baz") -> "foo/bar/baz.java".
    (apply string-append
	   (append (separate java.io.File.separator$ (crack name "."))
		   (list ".java"))))
  (define (make-src-file src-base java-file)
    (java.io.File. src-base (.toString java-file)))
  (define (open-src-file name)
    (let ((f (make-src-file dclass-src-base (class->file (.toString name)))))
      (display (string-append "Writing Java code for " name  " to " f "\n"))
      (.mkdirs (File. (.getParent f)))
      (java.io.PrintWriter. (java.io.BufferedWriter.
			     (java.io.FileWriter. f)))))
  (define (full-class-name class)
    (string-append (package class) "." (.name$ class)))
  (let* ((clauses (map parse-clause clauses))
	 (class (filter-class/1 dclass.Class.class clauses)))
    (.clauses$ class clauses)
    (let ((name (full-class-name class)))
      (let ((result (gen-class class))
	    (port (open-src-file name)))
	(emit result port)
	(.close port)
	(emit result (current-output-port))
	(display (string-append "Compiling Java code for " name "\n"))
	(out (run (cmd javac -classpath ,dclass-classpath
		       -d ,dclass-class-base
		       -sourcepath ,dclass-src-base
		       ,(make-src-file dclass-src-base
				       (class->file (.toString name))))))))))

(define (check-no-modifiers ms cs)
  (or (null? ms)
      (dclass-clause-error "Clause should not contain modifiers" ms cs)))

(define (check-length= n ms cs)
  (or (= (length cs) n)
      (dclass-clause-error "Clause has wrong number of components" ms cs)))

(define (check-length>= n ms cs)
  (or (>= (length cs) n)
      (dclass-clause-error "Clause has wrong number of components" ms cs)))

(define (dclass-clause-error msg ms cs) (error msg (append ms cs)))

(define (parse-clause clause)
  (if (not (pair? clause)) (error "clause should be a list " clause)
      (let* ((group (group-modifiers clause '()))
	     (modifiers (car group))
	     (clause (cdr group)))
	(case (car clause)
	  ((class) (make-class modifiers clause))
	  ((import) 
	   (and (check-no-modifiers modifiers clause)
		(check-length= 2 modifiers clause)
		(dclass.Import. (cadr clause))))
	  ((interface) (make-class modifiers clauses))
	  ((package) 
	   (and (check-no-modifiers modifiers clause)
		(check-length= 2 modifiers clause)
		(dclass.Package. (cadr clause))))
	  (else (parse-complex-clause modifiers clause))))))

(define (group-modifiers clause sofar)
  ;; (values modifiers rest-of-clause)
  (if (and (pair? clause) (modifier? (car clause)))
      (group-modifiers (cdr clause) (cons (car clause) sofar))
      (cons (reverse sofar) clause)))

(define (make-class modifiers clause)
  (check-length>= 2 modifiers clause)
  (let ((cori (car clause))
	(name (cadr clause))
	(clause (cddr clause)))
    (define (make-class0 extends implements)
      (dclass.Class. modifiers (eq? cori 'interface) name
		     extends implements))
    (define (make-extends clause extends implements)
      (cond ((null? clause) (make-class0 extends implements))
	    ((eq? (car clause) 'implements)
	     (make-implements (cdr clause) extends implements))
	    (else (make-extends (cdr clause)
				(cons (car clause) extends)
				implements))))
    (define (make-implements clause extends implements)
      (cond ((null? clause) (make-class0 extends implements))
	    ((eq? (car clause) 'extends)
	     (make-extends (cdr clause) extends implements))
	    (else (make-implements (cdr clause)
				   extends
				   (cons (car clause) implements)))))
    (if (null? clause)
	(make-class0 '() '())
	(cond
	 ((eq? (car clause) 'extends)
	  (make-extends (cdr clause) '() '()))
	 ((eq? (car clause) 'implements)
	  (make-implements (cdr clause) '() '()))
	 (else (error "clause is" clause))))))

(define (parse-complex-clause modifiers clause)
  ;; clause: ::=
  ;; type name                  -> field
  ;; type name = value          -> field
  ;; type name (arg ...) . body -> method
  ;; name (arg ...) . body      -> constructor
  ;; (exp) ...                  -> static
  (apply
   (cond
    ;; type name                  -> field
    ((and (symbol? (first clause)) (symbol? (second clause))
	  (= (length clause) 2))
     (lambda (type name) (dclass.Field. modifiers type name)))
    ;; type name = value          -> field
    ((and (symbol? (first clause)) (symbol? (second clause))
	  (= (length clause) 4) (eq? (third clause) '=))
     (lambda (type name ignore value)
       (dclass.Field. modifiers type name value)))
    ;; type name (arg ...) . body -> method
    ((and (symbol? (first clause)) (symbol? (second clause))
	  (list? (third clause)))
     (lambda (type name args . body)
       (dclass.Method. modifiers type name args body)))
    ;; name (arg ...) . body      -> constructor
    ((and (symbol? (first clause)) (list? (second clause)))
     (lambda (name args . body)
       (dclass.Constructor. modifiers name args body)))
    ((and (not (symbol? (first clause))) (equal? modifiers '(static)))
     (lambda body (dclass.Static. body)))
    (else
     (dclass-clause-error "Unknown clause syntax: " modifiers clause)))
   clause))

(define (invalid-clause modifiers clause)
  (error "Invalid clause " (append modifiers clause)))

(define (gen-class class)
  (define (gen-comment ignore)
    `("// Generated by Jscheme by " ,($ "user.name")
      " on " ,(.toString (java.util.Date.)) "\n"))
  (define (gen-package class) `(package ,(package class) ";"))
  (define (gen-imports class)
    (append 
     (map (lambda (i) `(import ,i ";")) (imports class))
     '((import jsint.Procedure ";")
       (import jscheme.JS ";"))))
  (define (gen-class-line class)
    (define (prefixing prefix data) (if (null? data) data (cons prefix data)))
    `(,@(.modifiers$ class)
      ,(if (.isInterface$ class) 'interface 'class)
      ,(.name$ class)
      ,@(prefixing 'extends (.extending$ class))
      ,@(prefixing 'implements (.implementing$ class))))
  (define (gen-class-body class)
    (map gen-member (members class)))

  (gensym-reset)
  `(,(gen-comment class)
    ,(gen-package class)
    ,@(gen-imports class)
    ,(gen-class-line class)
    "{" ,@(gen-class-body class) "}" ))

(define-method (gen-member (f dclass.Field))
  (cond
   ((eq? (.value$ f) dclass.Field.NOVALUE$)
    `(,@(.modifiers$ f) ,(.type$ f) ,(.name$ f) ";"))
   ((dclass-constant? (.value$ f))
    `(,@(.modifiers$ f) ,(.type$ f) ,(.name$ f)
      = ,(dclass-java-value (.value$ f)) ";"))
   (else
    (let* ((v (.value$ f))
	   (symbol (dclass-member-name "F_" f))
	   (proc (dclass-procedure
		  symbol (if (static? f) '() '(this)) (list v)))
	   (value (dclass-call (if (static? f) (list symbol)
				   (list symbol 'this))
			       (.type$ f))))
      (list 
       proc
       `(,@(.modifiers$ f) ,(.type$ f) ,(.name$ f)
	 = ,value ";"))))))
	  
(define (static? f) (member 'static (.modifiers$ f)))
(define (dclass-args m) (by 2 (.args$ m)))
(define (dclass-arg-names m) (map cadr (dclass-args m)))

(define (dclass-primitive? type)
  (member type '(boolean byte char short int long float double)))

(define (dclass-args-call m)
  ;; Argument to a JS.call must be objectified.
  (map (lambda (a)
	   (if (dclass-primitive? (car a))
	       `(JS.toObject "(" ,(cadr a) ")" )
	       (cadr a)))
	 (dclass-args m)))

(define (modifier? x) (member x '(
				  final
				  private
				  protected
				  public
				  static
				  strictfp
				  synchronized
				  transient
				  volatile
				  )))

(define (dclass-constant? x)
  ;; Is x a Scheme constant that is easy to convert to Java?
  (or (number? x) (string? x) (boolean? x)))

(define (dclass-java-value x)
  ;; Turn a Scheme constant into a string representing the Java value.
  (if (eq? x #null) 'null
      (dclass-java-value0 x)))
(define-method (dclass-java-value0 (x Boolean)) (if x "true" "false"))
(define-method (dclass-java-value0 (x Character)) (string-append #\' x #\'))
(define-method (dclass-java-value0 (x Float)) (string-append x "F"))
(define-method (dclass-java-value0 (x Long)) (string-append x "L"))
(define-method (dclass-java-value0 (x Number)) (.toString x))
(define-method (dclass-java-value0 (x Object))
  (error "Can't convert " x " to a Java value."))
(define-method (dclass-java-value0 (x String))
  (string-append "\"" (quotify x) "\""))

(define (quotify x)
  (define (find x n)
    (let ((end (.indexOf x "\"" n)))
      (if (= end -1) (.length x) end)))
  (define (quotify0 x start end so-far)
    (if (< end (.length x))
	(quotify0 x end (find x (+ end 1))
		  (cons "\\" (cons (.substring x start end) so-far)))
	(apply string-append (reverse (cons (.substring x start end)
					    so-far)))))
    (quotify0 x 0 (find x 0) '()))

(define (dclass-procedure symbol args body)
  (let ((p `(lambda ,args ,@body)))
    `(private static final Procedure ,symbol = "(" Procedure ")" 
	     JS.eval "(" ,(dclass-java-value (.toString p)) ")" ";")))

(define (test-stringify2)
  (let ((value '(string-append "x" "-" "y")))
    (dclass-java-value (.toString `(lambda () ,value)))))

(define (dclass-call args type)
  (let ((type (if (pair? type) (car type) type)))
    (if (or (eq? type 'void) (null? type))
	`(JS.call "(" ,@(separate "," args) ")")
	`(,(jsint->java type) "(" JS.call "(" ,@(separate "," args) ")" ")"))))

(define (jsint->java type)
  (let ((it (assoc type '(
			  (boolean JS.booleanValue)
			  (byte JS.byteValue)
			  (char JS.charValue)
			  (short JS.shortValue)
			  (int JS.intValue)
			  (long JS.longValue)
			  (float JS.floatValue)
			  (double JS.doubleValue)))))
    (if it (cadr it) (string-append "(" type ")"))))

(define (dclass-member-name prefix m)
  ;; Don't upcase name which might make it nonunique.
  (gensym
   (string->symbol
    (string-append "DCLASS_"
		   (if (equal? prefix "C_") ; Constructor.
		       "C_"
		       (string-append prefix (if (static? m) "S_" "I_")))
		   (.toString (.name$ m))))))

(define-method (gen-member (m dclass.Method))
  ;; +++ Need to handle this and super.
  (let ((member-name (dclass-member-name "M_" m)))
    (define (gen-method m)
      (let* ((args (dclass-args-call m))
	     (args (cons member-name
			 (if (static? m)
			     args
			     (cons 'this args)))))
	`(,@(.modifiers$ m) ,(.type$ m) ,(.name$ m)
	  "(" ,(separate "," (dclass-args m)) ")" "{"
	  ,@(if (not (eq? (.type$ m) 'void)) (list 'return) '())
	  ,(dclass-call args (.type$ m))
	  ";" "}")))
    (define (gen-static m)
      (dclass-procedure member-name
			(if (static? m)
			    (dclass-arg-names m)
			    (cons 'this (dclass-arg-names m)))
			(.body$ m)))

    (list
     (gen-method m)
     (gen-static m))))

(define-method (gen-member (clause dclass.Static))
  (let ((symbol (gensym "DCLASS_STATIC")))
    (list
     (dclass-procedure symbol '() (.item$ clause))
     `(static "{" ,(dclass-call (list symbol) 'void) ";" "}"))))

;;; Constructors and methods are the same except for the first form in
;;; their body.  Constructors can contain (this ...) or (super ...).
;;; The first form in a constructor body is called the "head".

(define-method (gen-member (clause dclass.Constructor))
  (let ((member-name (dclass-member-name "C_" clause)))
    (define (body-head? m)
      (let ((body  (.body$ m)))
	(and (not (null? body))
	     (pair? (car body))
	     (member (caar body) '(this super)))))
    (define (body-head m)
      (and (body-head? m) (car (.body$ m))))
    (define (body-tail m)
      (if (body-head? m) (cdr (.body$ m))
	  (.body$ m)))
    (define (gen-method m)
      ;; +++ Need to handle this and super.
      (let* ((args (dclass-args-call m))
	     (args (cons member-name (cons 'this args)))
	     (head (body-head m))
	     (tail (body-tail m)))
	`(,@(.modifiers$ m) ,(.name$ m)
	  "(" ,(separate "," (dclass-args m)) ")" "{"
	  ,(if head (gen-constructor-head head) '())
	  ,(if (null? tail) '()
	       `(,(dclass-call args 'void) ";"))
	  "}")))
    (define (gen-static m)
      (let ((tail (body-tail m)))
	(if (null? tail) '()
	    (dclass-procedure member-name
			      (cons 'this (map cadr (dclass-args m)))
			      tail))))
    (list
     (gen-method clause)
     (gen-static clause))))

(define (gen-constructor-head head)
  (define (head-simple? head)
    (every   (lambda (x)  (not (pair? x))) head))
  (if (head-simple? head)
      `(,(car head) "(" ,(separate "," (cdr head)) ")" ";")
      (error "Can't handle complex head " head)))

(define (test-2)
  (define-class
    (package frog)
    (import java.util.Comparator)
    (public class Compare implements Comparator)

    (private Procedure predicate)	

    (public Compare (Procedure predicate)
     (.predicate$# this predicate))

    (public boolean predicate (Object a Object b)
     ((.predicate$# this) a b))

    (public int compare (Object a Object b)
     (cond ((.predicate this a b) -1)
	   ((.predicate this b a) 1)
	   (else 0)))

    (public boolean equals (Object that)
     (and (eq? (.getClass this) (.getClass that))
	  (eq? (.predicate$# this) (.predicate$# that))))

    (public int hashCode () 0))
  (let ((a #(1 5 2 4 9 8 3 7 6 2 4)))
    (print a)
    (java.util.Arrays.sort a (frog.Compare. >))
    (print a)
    (java.util.Arrays.sort a (frog.Compare. <))
    (print a)
    #t))


;;;KRA 28JAN02: Currently super not handled.
(define (test-3)
  (define-class
    (package frog)
    (import java.util.Comparator)
    (public class Compare2 extends Compare)

    (public int predicateCount = 0)
    (public int compareCount = 0)
    (public Compare2 (Procedure predicate)
     (super predicate))
    (public boolean predicate (Object a Object b)
     (.predicateCount$ this (+ (.predicateCount$ this) 1))
     (.predicate super a b))
    (public int compare (Object a Object b)
     (.compareCount$ this (+ (.compareCount$ this) 1))
     (.compare super a b)))
  (let ((a #(1 5 2 4 9 8 3 7 6 2 4)))
    (print a)
    (java.util.Arrays.sort a (frog.Compare2. >))
    (print a)
    (print `(predicate: ,(.getPreciateCount a)
	     compare:   ,(.getCompareCount a)))))

'(define (decode-reflector name)
  (define (trim name)
    (define (trim-start name) (if (.startsWith name ".") 1 0))
    (define (trim-end name)
      (if (or (.endsWith name "$") (.endsWith name ".")) (- (.length name) 1)
	  (.length name)))
    (.substring name (trim-start name) (trim-end name)))

  (let* ((name (.toString name))
	 (iors (if (.startsWith name ".") 'instance 'static))
	 (m (if (.endsWith name "$") 'field
		(if (.endsWith name ".") 'constructor
		    'method))))
    (list (trim name) iors m)))
