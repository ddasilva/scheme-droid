{
Example of serializing objects, including procedures.
Originally from Toby Alsopp.
} 
(load "elf/util.scm")
(load "elf/iterate.scm")
(import "java.io.*")

(define (serialize filename x)
  ;; Serialize object, x, to file filename.
  (let ((oos (ObjectOutputStream. (FileOutputStream. filename))))
    (.writeObject oos x)
    (.close oos)))

(define (deserialize filename)
  ;; Inverse of serialize.  
  (let* ((ois (ObjectInputStream. (FileInputStream. filename)))
	 (x (.readObject ois)))
    (.close ois)
    x))

(define (saveHeap file)
  ;; Save the heap (symbol table) to file.
  (serialize
   file
   (foldL Symbol.symbolTable$
	  (lambda (s sofar)
	    (if (.isDefined s)
		(let ((value (.getGlobalValue s)))
		  (if (and (instanceof value Serializable.class)
			   (not (instanceof value Reflector.class))
			   (not (instanceof value Primitive.class)))
		      (cons (list s (.getGlobalValue s)) sofar)
		      sofar))
		sofar))
	  '())))

(define (restoreHeap file)
  ;; Restore the heap from file.
  (for-each (lambda (s) (.setGlobalValue (car s) (cadr s)))
	    (deserialize file)))
