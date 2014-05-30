;;;Extracting Links from an HTML File

;;; This is an example of reading HTML documents was originally from:
;;; http://developer.java.sun.com/developer/TechTips/1999/tt0923.html#tip1

;;; This version collects broken links.  For example:
'(walk-links (URL. "http://groupscheme.sourceforge.net")
	     collect-broken-links '() 2 (HashSet.))

(load "elf/basic.scm")
(import "java.io.FileReader")
(import "java.io.InputStreamReader")
(import "java.lang.Boolean")
(import "java.net.URL")
(import "java.net.URLConnection")
(import "javax.swing.text.Document")
(import "javax.swing.text.Element")
(import "javax.swing.text.ElementIterator")
(import "javax.swing.text.SimpleAttributeSet")
(import "javax.swing.text.html.HTML")
(import "javax.swing.text.html.HTML$Attribute")
(import "javax.swing.text.html.HTML$Tag")
(import "javax.swing.text.html.HTMLEditorKit")
(import "jlib.ExceptionHandler")

;;; Provide a HTTPS url parser.
;;; The proper jar's must be inserted in jre/lib/ext to actually work.
(System.setProperty "java.protocol.handler.pkgs"
		    "com.sun.net.ssl.internal.www.protocol")

(define (getReader url)
  (tryCatch (InputStreamReader. (.getInputStream (.openConnection url)))
	    (lambda (e) #null)))

(define (walk-links url how so-far level set)
  (define (read-doc kit rd doc)
    (tryCatch (begin (.read kit rd doc 0)
		     #t)
	      (lambda (e) (print e) #f)))
  (define (handle-element elem)
    (let ((s (.getAttribute (.getAttributes elem) HTML$Tag.A$)))
      (if (not (isNull s))
	  (let ((href (.getAttribute s HTML$Attribute.HREF$)))
	    (if (and (not (isNull href))
		     (not (= (.indexOf href "javascript:") 0))
		     (not (= (.indexOf href "mailto:") 0)))
		(let ((u (URL. url href)))
		  (print u)
		  (if (not (.contains set u))
		      (begin
			(.add set u)
			(set! so-far (how url href so-far))
			(set! so-far (walk-links u how so-far (- level 1)
						 set))))))))))
  (if (> level 0)
      (let* ((kit (HTMLEditorKit.))
	     (doc (.createDefaultDocument kit))
	     (rd (getReader url)))
	(if (not (isNull rd))
	    (begin
	      ;; KRA 28FEB02:
	      ;; The Document class does not yet handle charset's properly.
	      (.putProperty doc "IgnoreCharsetDirective" Boolean.TRUE$)
	      ;; Parse the HTML.
	      (if (read-doc kit rd doc)
		  ;; Iterate through the elements of the HTML document.
		  (iterate (ElementIterator. doc) handle-element))))))
  so-far)

(define (collect-broken-links page link so-far)
  (print page)
  (let* ((u (URL. page link))
	 (r (getReader u)))
    (display ".")
    (if (isNull r) (cons (print (list page link)) so-far)
	so-far)))
