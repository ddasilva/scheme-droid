;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  UNDER CONSTRUCTION
;;
;; CompileReflection.scm
;;
;; given a generic constructor or method
;; this creates a Java method _Generic_TYPE_XXX(Pair args)
;; of the form:
;; 
;;   1) examine the args to determine the particular method M[b]
;;      which best matches the arguments
;;   2) switch on b to code which coerces the args to the 
;;      appropriate type and directly calls the specified method/constructor
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import "jsint.*")
(import "java.lang.*")
(import "java.lang.reflect.*")


(define (demo1)
   (addImport "java.awt.*") 
   (.println System.out$ (compileGeneric 'constructor "GridLayout" "getFont" 121)))

(define (demo2)
   (addImport "java.awt.*") 
   (.println System.out$ (compileGeneric 'staticmethod "Font" "getFont" 122)))

(define (demo3)
   (addImport "java.awt.*") 
   (.println System.out$ (compileGeneric 'instancemethod "Container" "add" 123)))

(define (demo4)
   (addImport "jsint.*") 
   (.println System.out$ (compileGeneric 'staticfield "useJavaSyntax" "U" 124)))

(define (demo5)
   (addImport "jsint.*") 
   (.println System.out$ (compileGeneric 'instancefield "rest" "Pair"  125)))


;;; Here we need to translate from Wrapper to primitive types
;;; when we coerce the arguments.


;;(define (compileGeneric type classname methodname N)

(define (compileGeneric specifier N)
  (case (first specifier)
    ((staticfield) 
         (compileGenericField (first specifier) (second specifier) (third specifier) N))
    ((instancefield) 
         (if (and (= 3 (length specifier)) (> (.length (third specifier)) 0))
           (compileGenericField (first specifier) (second specifier) (third specifier) N)
           (jsint.E.error "the compiler requires instance fields to specify their class: (.CLASS.method args ...)")))
    ((constructor)
         (compileGenericMethod/Constructor (first specifier) "" (second specifier) N))
    ((staticmethod)
         (compileGenericMethod/Constructor (first specifier) (second specifier) (third specifier) N))
    ((instancemethod)
         (if (and (= 3 (length specifier)) (> (.length (third specifier)) 0))
           (compileGenericMethod/Constructor (first specifier) (second specifier) (third specifier) N)
           (jsint.E.error 
              "the compiler requires instance methods to specify their class: (.CLASS.method args ...)" 
               (U.stringify specifier))))
    (else
         (E.error  (string-append "Error in compileGeneric, unknown type " (U.stringify (list specifier N)))))))

(define (unwrap c X)
 (apply string-append
   (cond 
      ((equal? c boolean.class)
      `("(((Boolean) "   ,X ").booleanValue())"))
      ((equal? c byte.class)
      `("(((Byte) "      ,X ").byteValue())"))
      ((equal? c char.class)
      `("(((Character) " ,X ").charValue())"))
      ((equal? c short.class)
      `("(((Short) "     ,X ").shortValue())"))
      ((equal? c int.class)
      `("(((Integer) "   ,X ").intValue())"))
      ((equal? c long.class)
      `("(((Long) "      ,X ").longValue())"))
      ((equal? c float.class)
      `("(((Float) "     ,X ").floatValue())"))
      ((equal? c double.class)
      `("(((Double) "    ,X ").doubleValue())"))
      (else
      `("((" ,(getClassName c) ") " ,X ")")))))
    
(define (getClassName c)
   (cond ((.isPrimitive c)
          (.getName (primitiveWrapperType (primitiveClassNamed (.getName c)))))
         ((.isArray c) (string-append (.getName (.getComponentType c)) "[]"))
         (else (.getName c))))


(define (compileGenericField TYPE MEMBERNAME CLASSNAME N)
   (define c (classNamed CLASSNAME))
   (define f (.getField c MEMBERNAME))
   (define isFinal (Modifier.isFinal (.getModifiers f)))
   (define r (.getType f))
   (define RETURNTYPE (getClassName r))
   (define (getWrapperClassName c) (getClassName c))
  
  (cond ((equal? TYPE 'staticfield)
         (if (.isPrimitive r)
             (let ((WRAPPERCLASS (getWrapperClassName r)))
               (apply string-append
               `(                 
                  "public static " ,WRAPPERCLASS   " " ,(.toString N) 
                  "( Pair Args ){
                  if (Args.length() == 0) return new " ,WRAPPERCLASS "( " ,CLASSNAME "." ,MEMBERNAME ");\n"
               ,@(if isFinal ()  `("
                  else if (Args.length() == 1) 
                     return new " ,WRAPPERCLASS "(" ,CLASSNAME "." ,MEMBERNAME " 
                     = (" ,(unwrap r " Args.first" ) " ));\n"))
                  "
                  else {System.out.println(\"too many args in call to static field\" + \"" ,CLASSNAME "." ,MEMBERNAME "\");
                        return null;} }\n\n\n")))
            (apply string-append `(
               "public static " ,RETURNTYPE  " "
               ,(.toString N) 
               "( Pair Args ){\n"
               "    if (Args.length() == 0) return " ,CLASSNAME ".", MEMBERNAME ";\n"
              ,@(if isFinal ""  `(
               "    else if (Args.length() == 1) return (" 
                      ,CLASSNAME "." ,MEMBERNAME " = ((" ,RETURNTYPE ") Args.first ));\n"))
               "    else System.out.println(\"too many args in call to static field\" +\"" 
                      ,CLASSNAME "." ,MEMBERNAME "\"); \n"
               "    return null; }\n\n\n"))
           ))
        ((equal? TYPE 'instancefield)
         (if (.isPrimitive r)
           (let ((WRAPPERCLASS (getWrapperClassName r)))
               (string-append
                  "public static " WRAPPERCLASS  " " 
                   (.toString N) 
                   "( Pair Args ){
                   if (Args.length() == 1) return new "
                     WRAPPERCLASS"((( " CLASSNAME ") (Args.first))." MEMBERNAME ");
                   else if (Args.length() == 2) return 
                     new "  WRAPPERCLASS "(((( "CLASSNAME") (Args.first))." MEMBERNAME "
                      = ( " (unwrap  r "Args.second()") " )));
                   else {System.out.println(\"wrong number of args in call to static field\" + \"" 
                       CLASSNAME "." MEMBERNAME "\");
                   return null;} }\n\n\n")
               )
           (string-append
              "public static " RETURNTYPE " "
              (.toString N) 
                  "( Pair Args ){
             if (Args.length() == 1) return (( " CLASSNAME ") (Args.first))." MEMBERNAME ";
             else if (Args.length() == 2) return ((( "
                 CLASSNAME") (Args.first))." MEMBERNAME 
             " = ((" RETURNTYPE ") Args.second() ));
             else System.out.println(\"wrong number of args in call to static field\" + \"" 
                CLASSNAME "." MEMBERNAME "\"); 
                  return null; }\n\n\n")
           ))
         (else
         (E.error (string-append
            "Error in CompileReflection.scm -- unknown member type" (U.stringify (list TYPE CLASSNAME MEMBERNAME)))))))



(define (compileGenericMethod/Constructor type membername classname N)
  (define (makeListOfCases)
      (define c (classNamed classname))
      (define cs 
         (cond ((equal? type 'constructor) 
                (constructorTable c))
               ((equal? type 'instancemethod)
                (methodTable c membername #f))
               ((equal? type 'staticmethod)
                (methodTable c membername #t))))
      (define (makeCases I L)
        (if (null? L) ()
            (cons 
              (list I (first L) (second L))
              (makeCases (+ I 1) (rest(rest L))))))
      (makeCases 0 (array->list cs)))

  (define ListOfCases (makeListOfCases))

  (define (makeCompareArray L)
      (define (toBool B) (if B "true" "false"))
      (define (iter L1 L2)
          (define (makeCompareRow C1 L)
              (if (null? L) ()
              (if (null? (rest L))
                  `( ,(toBool (and (= (Array.getLength C1) (Array.getLength (second (first L))))
                           (moreApplicable C1 (second (first L))))))
                (append  
                  `( ,(toBool (and (= (Array.getLength C1) (Array.getLength (second (first L))))
                           (moreApplicable C1 (second (first L))))) ",")
                    (makeCompareRow C1 (rest L))))))
          (cond ((null? L1) ())
                ((null? (rest L1))
                 `("{" ,@(makeCompareRow (second (first L1)) L2)"}\n"))
                (else
                 (append 
                     `("{" ,@(makeCompareRow (second (first L1)) L2)"},\n")
                      (iter (rest L1) L2)))))
      (iter L L))



   (define (compileCompareCase N NthCase)
     (define (compareArgs I L)
       (if (null? L) '("true")
       (if (null? (rest L)) 
           `( "(args.nth(" ,I ") instanceof " ,(.toString (getClassName (first L))) ")")
         (append 
           `( "(args.nth(" ,I ") instanceof " ,(.toString (getClassName (first L))) ") &&
        ")
            (compareArgs (+ I 1) (rest L))))))
   `(
     "if ((numargs== " ,(Array.getLength (second NthCase)) ")
        &&
       ((b== -1) || (_moreApplicable_" ,N "[ " ,(first NthCase) "][b]))
        &&
       ("   ,@(compareArgs 0 (array->list (second NthCase))) ")) 
       { b = " ,(first NthCase) ";}
   
    "
   ))



  (define (compileSwitch type classname L)
      (define (makeArgList I L)
          (define (makeArg I c)
             (cond 
                ((equal? c boolean.class)
                `("(((Boolean) (args.nth(" ,I "))).booleanValue())"))
                ((equal? c byte.class)
                `("(((Byte) (args.nth(" ,I "))).byteValue())"))
                ((equal? c char.class)
                `("(((Character) (args.nth(" ,I "))).charValue())"))
                ((equal? c short.class)
                `("(((Short) (args.nth(" ,I "))).shortValue())"))
                ((equal? c int.class)
                `("(((Integer) (args.nth(" ,I "))).intValue())"))
                ((equal? c long.class)
                `("(((Long) (args.nth(" ,I "))).longValue())"))
                ((equal? c float.class)
                `("(((Float) (args.nth(" ,I "))).floatValue())"))
                ((equal? c double.class)
                `("(((Double) (args.nth(" ,I "))).doubleValue())"))
                (else
                `("(" ,(getClassName c) ") args.nth(" ,I ")"))))
    
          (if (null? L) ""
           `( 
             "\n            "
             ,@(makeArg I (first L)) 
             ,(if (not(null? (rest L))) "," " ")
            ,@(makeArgList (+ I 1) (rest L)))))

    (cond 
      ((equal? type 'constructor) 
             `(
          "    case " 
              ,(first L) 
              ": return new "
               ,(.getName (third L)) 
              "(" 
              ,@(makeArgList 0 (array->list (second L)))
              "); \n" ))
      ((or (equal? type 'staticmethod) (equal? type 'instancemethod))
       (let 
          ((returnType (.getReturnType (third L)))
           (invocation 
              (if (equal? type 'staticmethod)
                  `(,classname "." ,(.getName (third L)))
                  `("target." ,(.getName (third L))))))
        (cond 
         ((equal? void.class returnType)
             `(
          "    case " 
              ,(first L) 
              ": "
              ,@invocation
              "(" 
              ,@(makeArgList 0 (array->list (second L)))
              "); break; \n" ))
         ((.isPrimitive returnType)
             `(
          "    case " 
              ,(first L) 
              ": return new "
              ,(.getName (primitiveWrapperType returnType))
              "("
               ,@invocation
              "(" 
              ,@(makeArgList 0 (array->list (second L)))
              ")); \n" ))
         (else
             `(
          "    case " 
              ,(first L) 
              ": return "
              ,@invocation
              "(" 
              ,@(makeArgList 0 (array->list (second L)))
              "); \n" )))))
      (else "ERROR in CompileReflection.scm ")))
       

  (apply string-append
      `( 
"
static boolean [][] _moreApplicable_" ,N " = new boolean[][]{ 
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;    this array represents the boolean relation
;;    case I 'is more applicable than' case J
;;    and if false for incomparable cases
                                                               
       ,@(makeCompareArray ListOfCases)                                
                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Represents a generic Java member (method or constructor)
;;  which accepts its arguments in a list and uses the argument
;;  types to determine the most closely matching member, which
;;  is then called (after coercing the arguments appropriately).

"
};

public static "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                
       ,(if (equal? type 'constructor) classname "Object")    " "
       ,N  "(Pair args){\n"           
       ,(if (equal? type 'instancemethod)                     
            (string-append                                     
               classname                                       
               " target = (" classname                         
               ") args.first; args = (Pair) args.rest;\n")     
        "")                                                    
       "int numargs = args.length()" 
       ";\n"                        
      ,(if (equal? type 'constructor) classname "Object")     
                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
" 
    tmp=null;  // return type, for methods
    int b= -1; // index of best matching method/constructor

"
;;    determine best matching constructor by testing 
;;    the arguments for applicability and using the
;;    more-applicable partial order to find the 'best' match, b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                
      ,@(apply append                                           
         (map (lambda (x) (compileCompareCase N x)) ListOfCases))            
                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"
try {
switch (b) {  
   // jump to the best matching method/constructor
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                
      ,@(apply append                                          
         (map (lambda(x) (compileSwitch type classname x)) ListOfCases)) 
                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
"
   default: // error
}
} catch(Exception e) {throw new JschemeThrowable(e);}; 
 return tmp;
}

"
  ))

)


