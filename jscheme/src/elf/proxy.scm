;;;
;;; JDK 1.3 java.lang.reflect.Proxy support
;;; Uses elf.SchemeInvocationHandler.
;;; Examples from "http://www.cs.rice.edu/~matthias/Scheme2000/anderson.ps"
;;;
(load "elf/util.scm")

(import "elf.SchemeInvocationHandler")
(import "java.awt.*")
(import "java.lang.reflect.*")
(import "javax.swing.*")

(define (Proxy classLoader interface[] handler)
  ;; Create a Proxy for a Class[] of interfaces.
  ;; The handler has type (Proxy Method Object[] -> Object)
  (Proxy.newProxyInstance
   classLoader
   interface[]
   (SchemeInvocationHandler. handler)))

(define (demo)
  ;; A simple frame that demonstrates (Listener).
  (let ((f (JFrame. "Example"))
    	(b (JButton. "Press Me")))
    (.addActionListener 
     b
     (Listener (lambda (e) (set! *e* e) (print e))))
    (.add (.getContentPane f) b BorderLayout.CENTER$)
    (.pack f)
    (.show f)))

(define (interfaces class so-far)
  ;; Collect the interfaces of a class.
  (let ((is (.getInterfaces class)))
    (for-each* (lambda (i) (set! so-far (cons i so-far))) is)
    (let ((super (.getSuperclass class)))
      (if (eq? super #null) (reverse so-far)
	  (interfaces super so-far)))))

(define (listener-interfaces)
  (list->array Class.class
	       (map class
		    '("java.awt.event.AWTEventListener"
		      "java.awt.event.ActionListener"
		      "java.awt.event.AdjustmentListener"
		      "java.awt.event.ComponentListener"
		      "java.awt.event.ContainerListener"
		      "java.awt.event.FocusListener"
		      "java.awt.event.InputMethodListener"
		      "java.awt.event.ItemListener"
		      "java.awt.event.KeyListener"
		      "java.awt.event.MouseListener"
		      "java.awt.event.MouseMotionListener"
		      "java.awt.event.TextListener"
		      "java.awt.event.WindowListener"
		      "java.util.EventListener"
		      "javax.swing.event.AncestorListener"
		      "javax.swing.event.CaretListener"
		      "javax.swing.event.CellEditorListener"
		      "javax.swing.event.ChangeListener"
		      "javax.swing.event.DocumentListener"
		      "javax.swing.event.HyperlinkListener"
		      "javax.swing.event.InternalFrameListener"
		      "javax.swing.event.ListDataListener"
		      "javax.swing.event.ListSelectionListener"
		      "javax.swing.event.MenuDragMouseListener"
		      "javax.swing.event.MenuKeyListener"
		      "javax.swing.event.MenuListener"
		      "javax.swing.event.MouseInputListener"
		      "javax.swing.event.PopupMenuListener"
		      "javax.swing.event.TableColumnModelListener"
		      "javax.swing.event.TableModelListener"
		      "javax.swing.event.TreeExpansionListener"
		      "javax.swing.event.TreeModelListener"
		      "javax.swing.event.TreeSelectionListener"
		      "javax.swing.event.TreeWillExpandListener"
		      "javax.swing.event.UndoableEditListener"
		      ))))

(define (Listener handler)
  ;; Like jsint.Listener.class, returns a listener that implements the
  ;; above 35 interfaces by calling (handler event).
  ;; See (demo) for example.
  (let ((is (listener-interfaces)))
    (Proxy (.getClassLoader (vector-ref is 0))
	   is
	   (lambda (proxy method argv)
 	     (handler (vector-ref argv 0))))))

(define (delegate-to delegate handler)
  ;; Returns a proxy that delegates all its interface methods through handler
  ;; to delegate.  See (trace-object) for example.
  (Proxy.newProxyInstance
   (.getClassLoader (.getClass delegate))
   (.getInterfaces (.getClass delegate))
   (SchemeInvocationHandler.
    (lambda (proxy method argv)
      (handler delegate method argv)))))

(define (trace-handler delegate method argv)
  (print (list 'call: delegate (.getName method) argv))
  (let ((result (.invoke method delegate argv)))
    (print (list 'return: result))
    result))

(define (trace-object x)
  ;; Returns a proxy object for x that traces all interface methods.
  (delegate-to x trace-handler))

(define (comparator-handler compare)
  (lambda (this method argv)
    (let ((name (.getName method)))
      (cond ((.equals name "compare")
	     (let ((a (vector-ref argv 0))
		   (b (vector-ref argv 1)))
	       (compare a b)))
	    ((.equals name "equals")
	     (let ((that (vector-ref argv 0)))
	       (eq? this that)))
	    ((.equals name "toString")
	     (string-append "{" (.getName (.getClass this)) " " compare "}"))
	    ((.equals name "hashCode") (.hashCode compare))))))

(define (comparator compare)
  ;; Given compare a procedure of two arguments returns a Comparator.
  ;; See (sorting-example).
  (Proxy
   (.getClassLoader Comparator.class)
   (array Class.class Comparator.class)
   (comparator-handler compare)))

(define (sorting-example)
  ;; Demonstrates how Object[]'s can be sorted.
  (define c (comparator (lambda (a b)
			  (if (< a b) -1
			      (if (< b a) 1
				  0)))))

  (let ((x #(7 4 7 3 1 2 7))) (Arrays.sort x c) x)
  )