;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reflect.scm
;;
;; This program defines several procedures:
;;    (addImport name)
;;    (forName name)
;;    (classNamed name)
;;    (primitiveClassNamed name)
;;    (invokeConstructor CLASS ARGARRAY)
;;    (invokeMethod CLASS TARGET NAME ARGARRAY ISSTATIC)
;;    (invokeStatic CLASS NAME ARGARRAY)
;;    (invokeInstance TARGETOBJECT NAME ARGARRAY)
;;    (invokeRawConstructor CONSTRUCTOR ARGARRAY)
;;    (invokeRawMethod m target args)
;;    (constructorTable c)
;;    (methodTable c name isStatic)
;;    (methodArray v) 
;;    (primitiveWrapperType p)
;;    (isApplicable types args)
;;    (moreApplicable p1 p2)
;;    (findMethod methods args)
;;    (makeFindMethod methods args)
;;    (for-loop start stop action)
;;    (and-loop start stop test)
;; and it uses three variables
;;    importTable
;;    imports
;;    classTable
;;    BUCKET_SIZE
;;    constructorCache
;;    staticCache
;;    instanceCache
;;
;; This was translated from Ken Anderson's 
;;  Import.java and Invoke.java
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "jsint.*")
(import "java.lang.*")
(import "java.lang.reflect.*")
(import "java.util.Hashtable")
(import "java.util.Vector")

(define importTable (Hashtable. 20))
(define imports (Vector. 20))
(define classTable (Hashtable. 200))


(define (primitiveClassNamed name)
     (cond 
           ((.equals name "void")         Void.TYPE$)
           ((.equals name "boolean")   Boolean.TYPE$)
           ((.equals name "byte")         Byte.TYPE$)
           ((.equals name "char")    Character.TYPE$)
           ((.equals name "short")       Short.TYPE$)
           ((.equals name "int")       Integer.TYPE$)
           ((.equals name "long")         Long.TYPE$)
           ((.equals name "float")       Float.TYPE$)
           ((.equals name "double")     Double.TYPE$)
           (else #null)))


(define (addImport name)
  (define (Import name)
    (let ((wild (.endsWith name "*")))
      (list wild 
            (if wild 
               (.substring name 0 (- (.length name) (.length "*"))) 
                name))))
  (tryCatch
     (let ((x (.get importTable name)))
       (if (equal? x #null)
        (let ((y (Import name)))
             (begin
               (.put importTable name y)
               (.addElement imports y)))))
  (lambda(e) (.clear classTable))))
          

(define (forName name)
  (tryCatch (Import.forName name) (lambda(e) #null)))

(define (classNamed name)

  (define (classNamed0 name)
     (let ((c (primitiveClassNamed name)))
       (if (not (equal? c #null)) c (forName name))))

  (define (findClass package name)
     (let ((wild (first package))
           (prefix (second package)))
       (cond (wild 
              (forName (.concat prefix name)))
             ((.endsWith prefix (.concat "." name))
              (forName prefix))
             (else
              (forName name)))))

  (define (classNamed1 name i sofar)
    (if (< i 0)
        (if (equal? sofar #null)
            (E.error
	     (string-append
	      "Can't find " name " using " (U.stringify imports)))
            sofar)
    (begin
      (let ((c (findClass (.elementAt imports i) name)))
        (cond ((equal? c #null)
               (classNamed1 name (- i 1) sofar))
              ((or (equal? sofar #null) (equal? c sofar))
               (classNamed1 name (- i 1) c))
              (else
               (E.error (string-append 
                  "Ambiguous class name: " name " can be " sofar " or " c 
                  " + " (U.stringify imports)))))))))
        
  (define (storeClass c)
    (begin
       (.put classTable name c)
       c))

  (let ((result (.get classTable name)))
     (if (not (equal? result #null)) result
       (storeClass
         (if (.endsWith name "[]")
             (classNamedArray (.substring name 0 (- (.length name) (.length "[]"))))
             (let ((c (classNamed0 name)))
                (if (not (equal? c #null))
                     c
                    (classNamed1 name (- (.size imports) 1) #null))))))))



(define BUCKET_SIZE 2)

(define constructorCache (Hashtable. 200))
(define staticCache (Hashtable. 1000))
(define instanceCache (Hashtable. 1000))


(define (invokeRawConstructor m args)
  (tryCatch
    (.newInstance m args)
  (lambda(e)
    (E.error (U.stringify (list "Constructor Exception"  e m args))))))

(define (invokeRawMethod m target args)
  (tryCatch
    (.invoke m target args)
  (lambda(e)
    (E.error (U.stringify (list "Error in method application: " e m target args))))))


(define (invokeConstructor c args)
  (let ((ms (constructorTable c)))
    (invokeRawConstructor
        (findMethod ms args)
        args)))

(define (constructorTable c)
        (define (store c result)
            (if (not (equal? result #null)) 
                result
                (begin
                   (let ((result (methodArray (.getConstructors c))))
                       (.put constructorCache c result)
                       result))))
        (let ((result (store c (.get constructorCache c))))
            (if (= (Array.getLength result) 0)
                (E.error (U.stringify (list "Constructor " c " has not methods")))
                result)))

(define (invokeStatic c name args)
  (invokeMethod c c name args #t))

(define (invokeInstance t name args)
  (invokeMethod (.getClass t) t name args #f))

(define (invokeMethod c target name args isStatic)
  (invokeRawMethod 
       (findMethod (methodTable c name isStatic) args) 
       target args))

(define (methodTable c name isStatic)
        (define (methodTableLookup c name isStatic)
            (define (methodTableLookupStatic c name)
              (let* ((ms (.getMethods c))
                     (result (Vector. (Array.getLength ms)))
                     (theName (.toString name)))
                (for-loop 0 (Array.getLength ms) (lambda (i)
                     (let ((m (Array.get ms i)))
                        (if (and (Modifier.isStatic (.getModifiers m))
                                 (.equals (.getName m) theName))
                            (.addElement result m)))))
                (let ((result1 (Array.newInstance Object.class (.size result))))
                  (.copyInto result result1)
                  (methodArray result1))))
            (define (methodTableLookupInstance c name)
                (let* ((theName (.toString name))
                       (result (methodVector c theName))
                       (result1 (Array.newInstance Object.class (.size result))))
                   (.copyInto result result1)
                   (methodArray result1)))
    
    
            (define (methodVector c name)
                (define (methodVectorMerge c name result)
                    (let  ((s (.getSuperclass c))
                           (is (.getInterfaces c))
                           (ms (.getMethods c)))
                      (if (not (equal? s #null))
                          (methodVectorMerge s name result))
                      (for-loop 0 (Array.getLength is) (lambda (i)
                          (methodVectorMerge (Array.get is i) name result)))
                      (for-loop 0 (Array.getLength ms) (lambda (i)
                        (let ((m (Array.get ms i)))
                          (if (and (not (Modifier.isStatic (.getModifiers m)))
                                   (.equals (.getName m) name))
                              (maybeAdd result m)))))
                      result))
               (methodVectorMerge c name (Vector. 10)))
    
            (define (maybeAdd result m1)
                (define (parameterTypesMatch p1 p2)
                  (and
                    (equal? (Array.getLength p1) (Array.getLength p2))
                    (and-loop 0 (Array.getLength p1) (lambda(i)
                       (.equals (Array.get p1 i) (Array.get p2 i))))))
                (if (and-loop 0 (.size result) (lambda (i)
                       (let ((m2 (.elementAt result i)))
                          (not (parameterTypesMatch
                                 (.getParameterTypes m1)
                                 (.getParameterTypes m2))))))
                    (.addElement result m1)
                    #f))
    
            (if isStatic 
                (methodTableLookupStatic c name)
                (methodTableLookupInstance c name)))
    
    
      (define (methodTable0 c name isStatic)
          (define (getNameTable table name)
            (let ((nameTable (.get table name)))
              (if (not (equal? nameTable #null))
                  nameTable
                  (let ((newTable (Hashtable. 10)))
                       (.put table name newTable)
                        newTable))))
          (define (getMethodCache isStatic)
              (if isStatic staticCache instanceCache))
          (define (getCachedMethodTable c name isStatic)
              (.get (getNameTable (getMethodCache isStatic) name)  c))
          (define (putCachedMethodTable c name isStatic value)
              (.put (getNameTable (getMethodCache isStatic) name)  c value))
          (let ((result (getCachedMethodTable c name isStatic)))
            (if (not (equal? result #null)) result
              (begin
                (let ((result1 (methodTableLookup c name isStatic)))
                  (putCachedMethodTable c name isStatic result1)
                  result1)))))
    
      (let ((result1 (methodTable0 c name isStatic)))
        (if (and (not (equal? result1 #null)) (> (Array.getLength result1) 0))
            result1
            (begin
              (if isStatic 
                  (E.error (U.stringify (list   "No Static method of type" (.getName c) "." name "...")))
                  (E.error (U.stringify (list "No Instance method of type" (.getName c) "." name "..."))))))))



(define (methodArray v) 
 (begin
  (define (store i n r)
    (if (= i n) r
        (let ((j (* i BUCKET_SIZE)))
           (Array.set r j       (.getParameterTypes (Array.get v i)))
           (Array.set r (+ j 1) (Array.get v i))
           (store (+ i 1) n r))))
  (let ((result (Array.newInstance Object.class (* BUCKET_SIZE (Array.getLength v)))))
     (store 0 (Array.getLength v) result)))
)

(define (primitiveWrapperType p)
   (cond 
     ((.equals      Byte.TYPE$ p) Byte.class)
     ((.equals      Long.TYPE$ p) Long.class)
     ((.equals     Float.TYPE$ p) Float.class)
     ((.equals     Short.TYPE$ p) Short.class)
     ((.equals    Double.TYPE$ p) Double.class)
     ((.equals   Boolean.TYPE$ p) Boolean.class)
     ((.equals   Integer.TYPE$ p) Integer.class)
     ((.equals Character.TYPE$ p) Character.class)))

(define (isApplicable types args)
  (define (isArgApplicable p a)
      (or (and (equal? a #null)
               (.isAssignableFrom Object.class p))
          (.isInstance p a)
          (and (.isPrimitive p)
               (.isInstance (primitiveWrapperType p) a))))

  (if (= (Array.getLength types) (Array.getLength args))
      (and-loop 0 (Array.getLength args) (lambda(i)
          (isArgApplicable (Array.get types i) (Array.get args i))))
      #f)
)


(define (moreApplicable p1 p2)
  (and-loop 0 (Array.getLength p1) (lambda(i)
      (.isAssignableFrom (Array.get p2 i) (Array.get p1 i)))))


; used in invokeConstructor and invokeMethod
(define (findMethod methods args)

  (define (mostApplicable m1 m2)
     (let ((p1 (Array.get methods m1)))
        (if (and (isApplicable p1 args)
                 (or (= m2 -1) (not(moreApplicable (Array.get methods m2) p1))))
            m1
            m2)))
  (define (search m n best)
    (if (= m n) best
        (let ((newbest (mostApplicable m best)))
          (search (+ BUCKET_SIZE m) n newbest))))

  (if (= (Array.getLength methods) BUCKET_SIZE)
      (Array.get methods 1)
      (let ((best (search 0 (Array.getLength methods) -1)))
         (if (!= best -1)
             (Array.get methods (+ 1 best))
             (E.error (U.stringify 
                     (list "no method with name " 
                           (.getName (Array.get methods 1))
                           " and args "
                           (array->list args))))))))


(define (makeFindMethod methods args)
  (define (moreApplicable p1 p2)
    (and-loop 0 (Array.getLength p1) (lambda(i)
        (.isAssignableFrom (Array.get p2 i) (Array.get p1 i)))))

  (define (mostApplicable m1 m2)
     (let ((p1 (Array.get methods m1)))
        (if (and (isApplicable p1 args)
                 (or (= m2 -1) (not(moreApplicable (Array.get methods m2) p1))))
            m1
            m2)))
  (define (search m n best)
    (if (= m n) best
        (let ((newbest (mostApplicable m best)))
          (search (+ BUCKET_SIZE m) n newbest))))

  (if (= (Array.getLength methods) BUCKET_SIZE)
      (Array.get methods 1)
      (let ((best (search 0 (Array.getLength methods) -1)))
         (if (!= best -1)
             (Array.get methods (+ 1 best))
             (E.error (U.stringify 
                     (list "no method with name " 
                           (.getName (Array.get methods 1))
                           " and args "
                           (array->list args))))))))


;; simple auxiliary procedures

(define (for-loop start stop action)
  (if (>= start stop) #t
    (begin
      (action start)
      (for-loop (+ 1 start) stop action))))

(define (and-loop start stop test)
  (if (>= start stop) #t
    (begin
      (if (test start) 
          (and-loop (+ 1 start) stop test)
          #f))))

