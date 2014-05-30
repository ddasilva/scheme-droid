;; Inspect.scm -- A simple JDK 1.2 inspector
;; by Ken R. Anderson
;;
;; usage --  (inspect object)
;;
;; ** to run in the Demorunner, press "eval"
;; then "clear" the program and evaluate the query 
;;   (inspect OBJECT)
;; for the OBJECT you want to inspect, e.g.
;;   (inspect (javax.swing.JButton. "hi"))
;; a descriptive window will pop up, clicking on
;; the value fields creates value inspectors.

(import "java.awt.*")
(import "java.lang.reflect.*")
(import "javax.swing.*")
(import "javax.swing.event.*")
(import "jsint.*")

(define (Vector . es)
  (let ((v (Vector. (length es))))
    (for-each (lambda (e) (.add v e)) es)
    v))

(define-method (foldL (xs Object) how so-far)
  (if (.isArray (.getClass xs))
      (foldL-array xs how so-far 0 (Array.getLength xs))
      (error "Don't know how to foldL " xs)))

(define (foldL-array xs how so-far start end)
  (if (< start end) 
      (foldL-array xs how (how (Array.get xs start) so-far) (+ start 1) end)
      so-far))

(define (foldL-fields c how so-far)
  (let ((fs (.getDeclaredFields c)))
    (AccessibleObject.setAccessible fs #t)
    (let ((so-far (foldL fs how so-far))
	  (super (.getSuperclass c)))
      (if (eq? super #null) so-far
	  (foldL-fields super how so-far)))))

(define (field-row f so-far object)
  (.addElement so-far
	       (Vector (Modifier.toString (.getModifiers f))
		       (.getName (.getType f))
		       (.getName f)
		       (.get f object)))
  so-far)

(define (handleSelectionEvent data e)
  (let ((lsm (.getSource e)))
    (if (and (not (.isSelectionEmpty lsm)) (not (.getValueIsAdjusting e)))
	(let* ((row (.getMinSelectionIndex lsm))
	       (r (.elementAt data row))
	       (v (.elementAt r (- (.size r) 1))))
	  (if (and (not (eq? v #null)) (not (.isPrimitive (.getClass v))))
	      (inspect v))))))

(define (inspect object)
  (cond 
   ((eq? object #null) object)
   ((.isArray (.getClass object))
    (inspect-layout object (inspect-array-data object)
		    (Vector "Index" "Value")))
   (else (inspect-layout object (inspect-object-data object)
			 (Vector "Modifiers" "Type" "Name" "Value")))))

(define (inspect-object-data object)
  (foldL-fields (.getClass object)
		(lambda (x so-far) (field-row x so-far object))
		(Vector. 20)))
		  

(define (inspect-array-data object)
  (let ((i 0))
    (foldL object
	   (lambda (x so-far)
	     (.addElement so-far (Vector i x))
	     (set! i (+ i 1))
	     so-far)
	   (Vector. 20))))

(define (inspect-layout object data columns)
  (let* ((frame (JFrame. (.toString object)))
	 (table (JTable. data columns))
	 (listener (Listener. (lambda (e) (handleSelectionEvent data e)))))
    (.addListSelectionListener (.getSelectionModel table) listener)
    (.add (.getContentPane frame) (JScrollPane. table) BorderLayout.CENTER$)
    (.pack frame)
    (.show frame)
    frame))
