{
<html>
 <head>
  <title>Record classes</title>
<link rel="stylesheet" href="style.css">
 </head>
 <body>
  <h1>Record classes</h1>
  <p>The macro <tt>(define-record)</tt> defines a simple record class.
  Here are two examples:
  <pre>
  (define-record plan.Word
    (fields
     (name String)
     (nSpam 0)
     (nHam 0)
     (probability -1.0)
     (deviance -1.0)))

  (define-record plan.Filter
    (imports java.util.Hashtable)
    (fields
     (nSpam 0)
     (nHam 0)
     (words Hashtable (Hashtable. 2000))))
  </pre>
  <p>The general form is
  <pre>
  (define-record name clause ...)
  </pre>
  where <tt>name</tt> is the full name of the Java class,
  and <tt>clauses</tt> take the forms:
  <dl>
   <dt>(imports class ...)</dt>
   <dd>specifices which classes should be imported.</dd>
   <dt>(fields field ...)</dt>
   <dd>specifies the fields for the class.
  A field can take thes forms:
  <dl>
  <dt>(name)</dt>
  <dd>Field named name of type Object, required in constructor.</dd>
  <dt>(name type)</dt>
  <dd>Field named name of type type, required in constructor.</dd>
  <dt>(name value)</dt>

  <dd>Field named name, of type (.getClass value), to value, which
must be a constant expression.</dd>

  <dt>(name type value)</dt>
  <dd>Field named name, of type type initialized to runtime value, value.</dd>
  </dl>
  <dt>...</dt>
  <dd>Any other clauses are assumed to be clauses that <tt>define-class</tt>
  understands.
  </dl>

  <p> Two constructors will be defined, one that takes the minimum
number of arguments, and one that takes all the arguments to
initialize all the fields, in the order the fields were defined.

  </p>You can construct an object and get or set a field in the usual
JScheme way:

  <pre>
> (import "plan.Word")
#t
> (define w (Word. "Paul"))
plan.Word@5d88a
> (.name$ w)
"Paul"
> (.nSpam$ w)
0
> (.nSpam$ w (+ (.nSpam$ w) 1))
0
> (.nSpam$ w)
1
> 
  </pre>

  <pre>
}
(load "dclass/dclass.scm")		; <a href="dclass.scm">source</a>
(define-macro (define-record oname . clauses)
  (let* ((imports (clauseNamed 'imports clauses))
	 (fields (clauseNamed 'fields clauses))
	 (other-clauses (filter (lambda (c)
				  (not (member (car c) '(imports fields))))
				clauses))
	 (parts (crackName oname))
	 (package (car parts))
	 (name (cadr parts))
	 (javaFields (map generateField fields)))
    `(define-class 
       ,@(if package `((package ,package)) '())
       (public class ,name implements java.io.Serializable)
       ,@(map (lambda (i) `(import ,i)) imports)
       ,@javaFields
       ,@(generateConstructors name javaFields)
       (public String toString ()
	       (!{} "(" ',oname ". " ,@(separate
		       " "
		       (map (lambda (f) `(,(fieldAccessor f) this))
			    javaFields)) ")"))
       ,@other-clauses
       )))

(define (clauseNamed name clauses)
  (let ((it (assoc name clauses)))
    (if it (cdr it) '())))

(define (crackName name)
  (let* ((name (.toString name))
	 (i (.lastIndexOf name ".")))
    (if (= i -1) (list #f (string->symbol name))
	(list (string->symbol (.substring name 0 i))
	      (string->symbol (.substring name (+ i 1)))))))

(define TYPEALIST
  (by 2 (list Boolean.class boolean.class
	      Byte.class byte.class
	      Short.class short.class
	      Integer.class int.class
	      Long.class long.class
	      Float.class float.class
	      Double.class double.class)))

(define (value->type it)
  (let* ((c (.getClass it)))
    (if (.isArray c) (string->symbol (arrayType c))
	(string->symbol
	 (.getName
	  (if (.isPrimitive c) c
	      (let ((type (assoc c TYPEALIST)))
		(if type (cadr type) c))))))))

(define (arrayType c)
  (let ((ct (.getComponentType c)))
    (if (.isArray ct)
	(string-append (arrayType ct) "[]")
	(string-append (.getName (.getClass ct)) "[]"))))

{
  </pre>
  Convert a field into <tt>`(public ,type ,name)</tt> or
  <tt>`(public ,type ,name = ,value)</tt>.
  <pre>
}
(define (generateField f)
  (let ((name (car f)))
    (case (length f)
      ((3) `(public ,(cadr f) ,name = ,(caddr f)))
      ((2) (let ((type (cadr f)))
	     (if (symbol? type)
		 (if (class type)
		     `(public ,type ,name)
		     (error {[type] must be a class.}))
		 (let ((value (cadr f)))
		   `(public ,(value->type value) ,name = ,value)))))
      ((1) `(public Object ,name)))))
     
{
   </pre>
   Accessors for canonical fields:
   <pre>
}

(define fieldType cadr)
(define fieldName caddr)
(define (fieldAccessor f) (string->symbol {.[(fieldName f)]$}))

{
   </pre>
   Generate the constructor.
   <pre>
}
(define (generateConstructors name fields)
  (define (constructor fields)
    `(public ,name ,(flatten (map (lambda (f) `(,(fieldType f) ,(fieldName f)))
				  fields))
	     ,@(map (lambda (f) `(,(fieldAccessor f) this ,(fieldName f)))
		    fields)))
  `(,(constructor fields)
    ,(constructor (filter (lambda (f) (< (length f) 5)) fields))))

{
  </pre>
 </body>
</html>
}

