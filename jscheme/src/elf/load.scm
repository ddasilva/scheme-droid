;;;
;;; Changes (load x) to use several strategies for finding a file:
;;; - a file relative to an element of the list load-roots
;;; - a resource, such as in a Jar file.
;;; - a URL.

;;; For example to start scheme:
;;; java -jar jschme.jar "(set! load-roots '(directory1 directory2))" load.scm

;;;
(import "jsint.InputPort")
(import "java.io.File")
(import "java.io.FileInputStream")
(import "java.net.URL")

;;; KRA 02JUN00: We don't have a way, yet, of getting
;;; Code or document base.
(define (isApplet)
  (.equals (.getName (.getThreadGroup (Thread.currentThread)))
	   "applet-jsint.SchemeApplet"))

(define (load x)
  (let ((it (openInputPort x)))
    (if it (jsint.Scheme.load it)
	(jsint.Scheme.load x))))		; Error out!

(define-method (openInputPort (name String))
  (let ((it (or (openFile name) (openResource name) (openURL name))))
    (if it (InputPort. it))))

(define (openFile name)
  ;; Open a file by searching for it.
  (let ((it (fullPath name)))
    (if it (FileInputStream. it))))

(define (find-path-from-roots name roots)
  (and (not (null? roots))
       (let* ((root (car roots))
	      (roots (cdr roots))
	      (it (File. (U.stringify root #f) name)))
	 (if (.exists it) (.toString it)
	     (find-path-from-roots name roots)))))

(define (fullPath name) 
    (and (not (isApplet))
	 (let ((it1 (File. name)))
	   (if (.exists it1) (.getAbsolutePath it1)
	       (if (.isDefined 'load-roots)
		   (find-path-from-roots name load-roots)
		   #f)))))

(define (openResource name)
  (let ((loader (.getClassLoader jsint.Scheme.class)))
    (if (eq? loader #null) (ClassLoader.getSystemResourceAsStream name)
	(let ((it (.getResourceAsStream loader name)))
	  (and (not (eq? it #null)) it)))))

(define (openURL name)
  (jsint.Procedure.tryCatch (lambda () (.openStream (URL. name)))
			   (lambda (e) #f)))

(define (add-load-root x)
  ;; Add a load-root that may be relative to existing roots.
  (let ((it (fullPath x)))
    (if it (set! load-roots (cons it load-roots)))))
