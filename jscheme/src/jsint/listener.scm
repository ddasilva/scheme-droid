;;; listener.scm
;;; Loading this file generates a Listener.java file representing a
;;; class that provides over 80 methods to implement 35 Listener
;;; interfaces for AWT and SWING.  All you need are the class names,
;;; reflection and a little Scheme.
;;;
;;; I think this should create three Listener files
;;; corresponding to JDK 1.1, JDK1.1+swing, JDK1.2
;;; JDK1.2 can extend 1.1+swing, which extends 1.1,
;;; So there won't be too much duplicate code.
;;; /**
;;;  * @author Ken R. Anderson, Copyright 2000, kanderso@bbn.com, <a href="license.txt">license</a>
;;;  * subsequently modified by Jscheme project members
;;;  * licensed under zlib licence (see license.txt)
;;;  */


;;;(import "java.io.*")
(import "java.io.File")
(import "java.io.FileOutputStream")
(import "java.io.PrintStream")

(define listener-data-11		; Listeners from awt 
  '(
    (java.awt.event
     (
      Action Adjustment  
      Component Container
      Focus 
      Item Key Mouse
      MouseMotion Text Window
      ))
))

(define listener-data-11swing		; Listeners from swing for JDK1.1
  '(
    (javax.swing.event  
     (
      Ancestor Caret CellEditor Change Document
      Hyperlink InternalFrame ListData ListSelection MenuDragMouse
      MenuKey Menu MouseInput PopupMenu TableColumnModel
      TableModel TreeExpansion TreeModel TreeSelection TreeWillExpand
      UndoableEdit
      ))
))


(define listener-data-12		; Listeners from awt and swing for JDK1.2
  '(
    (java.awt.event
     (
      AWTEvent ;;  JDK1.2 extension
      InputMethod  ;; JDK1.2 extension
      ))
))




(define (makeListener listener-data classname extends)
    
  (define out (PrintStream.
	       (FileOutputStream. (File. {src/jsint/[classname].java}))))

  (define (p x)
    (.println out x)
    x)

  (define (filter p xs)
    (if (null? xs) xs
	(if (p (car xs)) (cons (car xs) (filter p (cdr xs)))
	    (filter p (cdr xs)))))

  (define listeners
    ;; Raw list of listener interfaces.
    ;; In older environments some classes may not exist.
    (filter (lambda (c) (not (eq? c #null)))
	    (map					
	     class
	     (apply append 
		    (map (lambda (x) 
			   (let ((prefix (car x))
				 (classes (cadr x)))
			     (map (lambda (x) {[prefix].[x]Listener}) classes)))
			 listener-data)))))
    
  (define (interface-list cs so-far)
    ;; Given a list of interfaces, cs, return a unique set of
    ;; interfaces, including inherited interfaces.
    (define (adjoin x xs) (if (member x xs) xs (cons x xs)))
    (if (null? cs) so-far
	(let* ((c (car cs))
	       (cs (cdr cs))
	       (so-far (adjoin c so-far))
	       (is (.getInterfaces c)))
	  (if (eq? is #null) (interface-list cs so-far)
	      (interface-list (append (vector->list is) cs) so-far)))))
    
  (define (separate xs by)
    (define (separate0 x xs by)
      (if (null? xs) (list x)
	  (cons x (cons by (separate0 (car xs) (cdr xs) by)))))
    (if (null? xs) '()
	(separate0 (car xs) (cdr xs) by)))
    
  (define (genmethod m)
    (p (string-append "  public void " (.getName m) "(" 
		      (.getName (vector-ref (.getParameterTypes m) 0)) " e" ") {
        " "handler.apply(U.list(e));
      }
    ")))
    
  (define (genlistener interfaces)
    (let ((implements (if (null? interfaces) ""
			  (apply string-append
				 (cons " implements "
				       (separate (map .getName interfaces)
						 ", "))))))
    (p "package jsint;")
    (p "")
    (p {/** This class allows a Procedure to act as a listener to many events.
    
	    For example, to add an action listener to a button, b:
    
	    <pre>
	    (import "java.awt.*")
	    (import "javax.swing.*")
	    (let ((f (JFrame. "Example"))
		  (b (JButton. "Press Me")))
	      (.addActionListener 
	       b
	       ([classname]. (lambda (e) (.println (System.out$) "Yow!"))))
	      (.add (.getContentPane f) b (BorderLayout.CENTER$))
	      (.pack f)
	      (.show f))
	    </pre>
	    NOTE: [classname].java IS GENERATED FROM listener.scm. EDIT AT YOUR OWN
	    RISK.
	    **/ 
	    })
    (p (apply string-append
	      `("public class " ,classname " extends " ,extends
		,implements "{")))
    (p {  public [classname](Procedure handler) \{})
    (p "    super(handler);")
    (p "  }")
    (p " ")
    (for-each 
     (lambda (m) (genmethod m))
     (apply append (map vector->list (map .getDeclaredMethods interfaces))))
    (p "}")))
    
  ;; (.println System.out$ `(makeListener ,listener-data ,classname ,extends))
  ;; (.println System.out$ listeners)
  (genlistener (interface-list listeners '()))
  (.println System.out$ (string-append "generated " classname ".java"))
    
  (.close out)
  )    

(makeListener listener-data-11 "Listener11" "JavaListener")
(makeListener listener-data-11swing "Listener11swing" "Listener11")
(makeListener listener-data-12 "Listener" "Listener11swing")

