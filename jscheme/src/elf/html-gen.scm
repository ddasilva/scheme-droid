;;;
;;; Simple HTML Generation.
;;;

;Syntax of HTML Tags From:

;HTML 3.0                                                       28th March 1995

;   INTERNET DRAFT                                 Dave Raggett, W3C
;   Expires in six months                          email: <dsr@w3.org>

;             HyperText Markup Language Specification Version 3.0

;                       <draft-ietf-html-specv3-00.txt>
;...
;Names

;   The element name immediately follows the tag open delimiter. An
;   element name consist of a letter followed by up to 72 letters,
;   digits, periods, or hyphens. Names are not case sensitive. For
;   example, H1 is equivalent to h1. This limit of 72 characters is set
;   by the NAMELEN parameter in the SGML declaration for HTML 3.0. 
;;; name = a[ad.-]*<=72
;Attributes

;   In a start tag, white space and attributes are allowed between the
;   element name and the closing delimiter. An attribute typically
;   consists of an attribute name, an equal sign, and a value (although
;   some attributes may be just a value). White space is allowed around
;   the equal sign. 

;;; attrib = name w* "=" w* value | value

;   The value of the attribute may be either: 

;   1.  A string literal, delimited by single quotes or double quotes 

;;; stringLiteral = delimited(many(StringChar), alt("\"", "'"))

;   2.  A name token (a sequence of letters, digits, periods, or
;       hyphens) 

;;; nameToken = many([ad.-])

;   In this example, a is the element name, href is the attribute name,
;   and http://host/dir/file.html is the attribute value: 

;       <A HREF="http://host/dir/file.html">

;   Some implementations consider any occurrence of the > character to
;   signal the end of a tag. For compatibility with such
;   implementations, when > appears in an attribute value, you may want
;   to represent it with an entity or numeric character reference, such
;   as: 

;;; value = escaped(alt(stringLiteral, nameToken))

;       <IMG SRC="eq1.ps" alt="a &#62; b">

;   To put quotes inside of quotes, you can use single quotes if the
;   outer quotes are double or vice versa, as in: 

;       <IMG SRC="image.ps" alt="First 'real' example">


;   Alternatively, you use the character representation &quot; as in: 

;       <IMG SRC="image.ps" alt="First &quot;real&quot; example">

(load "elf/basic.scm")

;;; Check for needs-quote
(define (value-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (eqv? c #'.') (eqv? c #'-')))

(define (needs-quoting? v start end) 
  (and (< start end)
       (or (not (value-char? (string-ref v start)))
	   (needs-quoting? v (+ start 1) end))))

(define (maybe-quote-value v)
  (define (html-quote v)
    (let ((sb (StringBuffer. (* 4 (string-length v)))))
      (.append sb #'"')
    (iterate v (lambda (c) (.append sb (character->html-character c))))
    (.append sb #'"')
      (.substring sb 0)))		; Shorten it.
  (let ((v (U.stringify v #f)))
    (let ((start 0)
	  (end (string-length v)))
      (if (needs-quoting? v start end) (html-quote v)
	  v))))

(define (character->html-character c)
  "Certain characters must be converted into entities."
  (case c
    ((#'>')  "&gt;")
    ((#'<')  "&lt;")
    ((#'&')  "&amp;")
    ((#'\'') "&apos;")
    ((#'"')  "&quot;")
    (else c)))

;   The length of an attribute value (after replacing entity and numeric
;   character references) is limited to 1024 characters. This number is
;   defined by the LITLEN parameter in the SGML declaration for HTML
;   3.0. 

;   Note: Some implementations allow any character except space or > in
;   a name token. Attributes values must be quoted only if they don't
;   satisfy the syntax for a name token. 

;   Attributes with a declared value of NAME (e.g. ISMAP, COMPACT) may
;   be written using a minimized syntax. The markup: 

;       <UL COMPACT="compact">

;   can be written as: 

;       <UL COMPACT>

;   Note: Unless you use the minimized syntax, some implementations
;   won't understand. 


;;; The idea is to keep things very simple.  
;;; HTML output constructed as a string.
;;; Tags are named by symbols.  
;;; Most optimization opportunities are ignored.

;;; The only real optimization is that (tag) constructs a list of
;;; strings that are string-appended together all at once.
;;; Nested (tag)'s construct intermediate substrings.

(define (tag tag . xs)
  ;; Write a tag with contents, xs.  TAG can be:
  ;; tag -> name
  ;; tag -> (name . attribute ...)
  ;; name -> a symbol naming an HTML element, like 'h1.
  ;; attribute -> a symbol naming an attribute with a declare value, 
  ;;              like 'compact.
  ;; attribute -> (name value)
  ;; value -> will be turned into a string and escaped properly.
  (let ((tag-name (if (pair? tag) (car tag) tag))
	(attributes (if (pair? tag) (cdr tag) '())))
    (apply string-append 
	   `("<" ,tag-name ,@(if (null? attributes) '()
				 (cons " " (tag-attributes attributes))) 
	     ">" 
	     ,@(flatten xs)
	     "</" ,tag-name ">"))))

(define (tag-attributes atts)
  ;; "Write the attributes of a tag."
  (if (null? atts) '()
      (let ((att (car atts))
	    (atts (cdr atts)))
	 (if (pair? att)
	  (let ((name (car att))
		(value (cadr att)))
	    `(,name "=" ,(maybe-quote-value value) 
		    ,@(if (null? atts) '()
			  (cons " " (tag-attributes atts)))))
	  (cons att (tag-attributes (cdr atts)))))))

(define (nl x) (string-append x "\n"))

(define (<>s . xs) (apply string-append (flatten xs)))

(define-macro (<> head . xs)
  ;; Like tag, but first argument is automatically quasiquoted.
  `(tag ,(list 'quasiquote head) ,@xs))

;;; Example
'(begin
   (import "java.io.File")
   (define (yesify x) (if x "yes" "no"))
   (define (directory-listing file border-size)
     ;; Directory listing of file.
     (define (row f)
       (nl (<> tr
             (<> td
               (let ((x (.getName f)))
                 (if (.isDirectory f)
                     (<> (a (href ,(.toURL f))) x)
                     x)))
             (<> td (.length f))
             (<> td (Date. (.lastModified f)))
             (<> td (yesify (.canRead f)))
             (<> td (yesify (.canWrite f))))))
     (<>s
      (<> head (<> 'title "Directory "file))
      (<> body
        (<> (table (border ,border-size))
          (<> caption (<> em "Directory listing of " file))
          (<> tr (map (lambda (x) (<> th x)) 
                      '(Name Length "Last Modified" Readable Writeable)))
          (map* row (.listFiles file))
          ))))

   )					; end begin