;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BootstrapR4RSa.scm
;;   this contains a definition of the Scheme R4RS primitives 
;;   in "core" Jscheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first we import the BootstrapCore.scm procedures and macros into a fresh environment

(.interactionEnvironment$ (Scheme.currentEvaluator) (jsint.DynamicEnvironment.))

;; then we load in the jscheme/BootstrapCore.scm module which defines the most basic procedures

(.environmentImport (Scheme.currentEvaluator) "jscheme/BootstrapCore.scm" #f #f #null) ; load in all procedures
(.environmentImport (Scheme.currentEvaluator) "jscheme/BootstrapCore.scm" #f #t #null) ; load in all macros


(define (import S) (begin (Import.addImport (.toString S)) #t))
(import "jsint.*")
(import "jscheme.*")


;;  (// "========== SECTION 6.1 BOOLEANS ==========")
(define (not x) (.equals #f x))
(define (boolean? x) (.isInstance Boolean.class x))

;;  (// "========== SECTION 6.2 EQUIVALENCE PREDICATES ==========")
(define eqv? Op.eqv)

;; here we define eq? to hold for all scalar quantities
(define (eq? x y) 
  (or 
     (Op.sameObject x y) 
     (and (or (.isInstance java.lang.Character.class x) 
              (.isInstance java.lang.Boolean.class x) 
              (.isInstance java.lang.Number.class x) 
            )
         (.equals x y))))

          ;; two objects are equal if they are 
          ;;   lists of equals, or arrays of equals, 
          ;;   or are eqv?
(define (equal? x y)
  (define (eqpair? x y)                  
     (and (equal? (first x) (first y)) 
          (equal? (rest x) (rest y))))
  (define (eqarray? x y)
     (and (eqv? (java.lang.reflect.Array.getLength x) (java.lang.reflect.Array.getLength y))
          (eqarrayiter? (- (java.lang.reflect.Array.getLength x) 1) x y)))
  (define (eqarrayiter? N x y)
     (if (< N 1) #t
     (if (equal? (java.lang.reflect.Array.get x N) (java.lang.reflect.Array.get y N))
         (eqarrayiter? (- N 1) x y)
         #f)))
  (cond ((eqv? x y) #t)
        ((and (pair? x) (pair? y)) 
         (eqpair? x y))
        ((and (.isArray (.getClass x)) (.isArray (.getClass y)))
         (eqarray? x y))
        (else #f)))
         
;;  (// "========== SECTION 6.3 LISTS AND PAIRS ==========")
;; most are defined in BootstrapCore.scm
;;
;; pair? cons car cdr first rest null? list length append reverse list-tail list-ref
;;  second third fourth fifth sixth seventh eight ninth tenth
;;   caar cadr cdar cddr
;;     caaar  caadr  cadar         cdaar  cdadr  cddar  cdddr
;;    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
;;    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr) 1)

(define (set-car! L v) (.first$ L v))
(define (set-cdr! L v) (.rest$ L v))

(define (list? L)
  (if (null? L) #t
      (and (pair? L) (list? (cdr L)))))


(define (memq x L)
  (cond ((null? L) #f)
        ((not(pair? L)) #f)
        ((eq? x (first L)) L)
        (else (memq x (cdr L)))))

(define (memv x L)
  (cond ((null? L) #f)
        ((not(pair? L)) #f)
        ((eqv? x (first L)) L)
        (else (memv x (cdr L)))))

(define (member x L)
  (cond ((null? L) #f)
        ((not(pair? L)) #f)
        ((equal? x (first L)) L)
        (else (member x (cdr L)))))

(define (assq x L)
  (cond ((null? L) #f)
        ((eq? x (first (first L))) (first L))
        (else (assq x (rest L)))))

(define (assv x L)
  (cond ((null? L) #f)
        ((eqv? x (first (first L))) (first L))
        (else (assv x (rest L)))))

(define (assoc x L)
  (cond ((null? L) #f)
        ((equal? x (first (first L))) (first L))
        (else (assoc x (rest L)))))

;;  (// "========== SECTION 6.4 SYMBOLS ==========")
;;  predefined in BootstrapCore.scm
;;     symbol? 

(define (symbol->string S) (.toString S))
(define (string->symbol S) (Symbol.intern S))

;;  (// "========== SECTION 6.5 NUMBERS ==========")
;;  predefined in BootstrapCore.scm
;;     number? = < > <= >= + * - / max min 
;;     % & ^ | ~ << >> >>> == != 
;;

(define (integer? x)
  (or (byte? x) (short? x) (int? x)))

(define (real? x)
  (or (float? x) (double? x)))

(define exact? integer?)
(define inexact? real?)

(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd?  x) (= 1 (% x 2)))
(define (even? x) (= 0 (% x 2)))


(define abs java.lang.Math.abs)
(define quotient Op.div)
(define remainder Op.mod)
(define (modulo x y) 
   (let ((z (Op.mod x y))) 
        (if (or (equal? (> y 0) (> z 0)) (= z 0)) z (+ z y))))

(define gcd 
  (letrec ((gcd2 (lambda (x y) (if (< x y) (gcd2 y x) (if (= y 0) x (gcd2 y (% x y))))))
           (iter (lambda (x L) (if (null? L) x (if (= x 1) 1 (iter (gcd2 (.longValue x) (.longValue (abs (first L)))) (rest L)))))))
  (lambda L
    (iter (abs (first L)) (rest L)))))

(define lcm
  (letrec ((lcm2 (lambda (x y) (/ (* x y) (gcd x y))))
           (iter (lambda (x L) 
                    (if (null? L) x (iter (lcm2 (.longValue x) (.longValue (abs (first L)))) (rest L))))))
    (lambda L (iter 1 L))))

;;  (// "inessential numerator, denominator, rationalize not implemented")

(define (floor x) (java.lang.Math.floor (.doubleValue x)))
(define (ceiling x) (java.lang.Math.ceil (.doubleValue x)))
(define (truncate x) (if (< x 0) (ceiling x) (floor x)))
(define (round x) (java.lang.Math.round (.doubleValue x)))

(define (exp x) (java.lang.Math.exp (.doubleValue x)))
(define (log x) (java.lang.Math.log (.doubleValue x)))
(define (sin x) (java.lang.Math.sin (.doubleValue x)))
(define (cos x) (java.lang.Math.cos (.doubleValue x)))
(define (tan x) (java.lang.Math.tan (.doubleValue x)))
(define (asin x) (java.lang.Math.asin (.doubleValue x)))
(define (acos x) (java.lang.Math.acos (.doubleValue x)))
(define (atan x) (java.lang.Math.atan (.doubleValue x)))
(define (sqrt x) (java.lang.Math.sqrt (.doubleValue x)))
(define (expt x y) (java.lang.Math.pow (.doubleValue x) (.doubleValue y)))

;;  (// "inessential complex arithmetic not implemented")
(define (exact->inexact x) (.doubleValue x))
(define (inexact->exact x) (.intValue x))

(define string->number
  (lambda L
    (if (= (length L) 2) 
        (if (= (second L) 10) 
            (java.lang.Double. (first L))
            (java.lang.Long.parseLong (first L) (second L)))
        (java.lang.Double. (first L)))))

(define number->string  
  (lambda L
    (if (= (length L) 2) 
        (if (= (second L) 10)
            (.toString (first L))
            (java.lang.Long.toString (first L) (second L)))
        (.toString (first L)))))
                        
;;  (// "========== SECTION 6.6 CHARACTERS ==========")

(define (char? x) (.isInstance java.lang.Character.class x))
(define (char=? x y) (eqv? x y))
(define (char<? x y) (< (Op.charToNumber x) (Op.charToNumber y)))
(define (char>? x y) (> (Op.charToNumber x) (Op.charToNumber y)))
(define (char<=? x y) (<= (Op.charToNumber x) (Op.charToNumber y)))
(define (char>=? x y) (>= (Op.charToNumber x) (Op.charToNumber y)))

(define (char-ci=? x y) (= (Op.charToNumber (java.lang.Character.toUpperCase x)) (Op.charToNumber (java.lang.Character.toUpperCase y))))
(define (char-ci<? x y) (< (Op.charToNumber (java.lang.Character.toUpperCase x)) (Op.charToNumber (java.lang.Character.toUpperCase y))))
(define (char-ci>? x y) (> (Op.charToNumber (java.lang.Character.toUpperCase x)) (Op.charToNumber (java.lang.Character.toUpperCase y))))
(define (char-ci<=? x y) (<= (Op.charToNumber (java.lang.Character.toUpperCase x)) (Op.charToNumber (java.lang.Character.toUpperCase y))))
(define (char-ci>=? x y) (>= (Op.charToNumber (java.lang.Character.toUpperCase x)) (Op.charToNumber (java.lang.Character.toUpperCase y))))
(define char-alphabetic? java.lang.Character.isLetter)
(define char-numeric?  java.lang.Character.isDigit)
(define char-whitespace? java.lang.Character.isWhitespace)
(define char-upper-case? java.lang.Character.isUpperCase)
(define char-lower-case? java.lang.Character.isLowerCase)
(define char->integer Op.charToNumber)
(define integer->char Op.numberToChar)
(define char-upcase java.lang.Character.toUpperCase)
(define char-downcase java.lang.Character.toLowerCase)

;;  (// "========== SECTION 6.7 STRINGS ==========")

(define (string? x) (.isInstance java.lang.String.class x))
(define make-string 
 (letrec ((iter (lambda (I N C B) (if (< I N) (begin (.insert B I C) (iter (+ I 1) N C B)) B))))
  (lambda L 
    (if (null? (cdr L)) 
       (java.lang.String. (java.lang.reflect.Array.newInstance char.class (first L))) 
       (java.lang.String. (iter 0 (first L) (second L) (java.lang.StringBuffer. (first L))))))))

(define string (lambda L (java.lang.String. (list->array java.lang.Character.class L))))

(define string-length .length)
(define string-ref .charAt)
(define string-set! "UNIMPLEMENTED!")
(define string=? .equals)
(define (string-ci=?  x y) (.equals (.toUpperCase x) (.toUpperCase y)))
(define (string<? x y) (< (.compareTo x y) 0))
(define (string>? x y) (> (.compareTo x y) 0))
(define (string<=? x y) (<= (.compareTo x y) 0))
(define (string>=? x y) (>= (.compareTo x y) 0))
(define (string-ci<? x y) (< (.compareTo (.toUpperCase x) (.toUpperCase y)) 0))
(define (string-ci>? x y) (> (.compareTo (.toUpperCase x) (.toUpperCase y)) 0))
(define (string-ci<=? x y) (<= (.compareTo (.toUpperCase x) (.toUpperCase y)) 0))
(define (string-ci>=? x y) (>= (.compareTo (.toUpperCase x) (.toUpperCase y)) 0))
(define (substring s x y) (.substring s x y))
(define string-append 
  (lambda L
     (let loop ((L L) (B (java.lang.StringBuffer.)))
         (if (null? L) (.toString B)
             (loop (rest L) (.append B (first L)))))))

(define (string->list L) (array->list (.toCharArray L)))
(define (list->string L) (java.lang.String. (list->array char.class L)))

;;  (// "Inessential string-copy, string-fill! not implemented")
;;  (// "========== SECTION 6.8 VECTORS ==========")

(define (vector? v) (.isArray (.getClass v)))

;;  (// "Inessential vector-fill! and (make-vector k fill) not implemented")
(define make-vector
  (lambda R
     (case (length R)
       ((1) (java.lang.reflect.Array.newInstance Object.class (first R)))
       ((2) (let ((v (second R))
                  (n (first R))
                  (z (java.lang.reflect.Array.newInstance Object.class (first R))))
              (let loop ((i 0)) 
                 (if (< i n) 
                    (begin (java.lang.reflect.Array.set z i v) (loop (+ i 1)))
                    z))))
       (else (throw "Error in make-vector. Must have 1 or 2 args")))))

;(define (make-vector n) (java.lang.reflect.Array.newInstance Object.class n))
(define vector (lambda L (list->array Object.class L)))
(define (vector-length v) (java.lang.reflect.Array.getLength v))
(define (vector-ref v n) (java.lang.reflect.Array.get v n))
(define (vector-set! v n x) (java.lang.reflect.Array.set v n x))
(define (vector->list L) (array->list L))
(define (list->vector L) (list->array Object.class L))

;;  (// "========== SECTION 6.9 CONTROL FEATURES ==========")
(define (procedure? x) (.isInstance Procedure.class x))
(define (apply x y) (.apply x y))

(define map 
 (letrec ((firsts (lambda (L) (if (null? L) () (cons (first (first L)) (firsts (rest L))))))
          (rests  (lambda (L) (if (null? L) () (cons (rest  (first L)) (rests  (rest L)))))))
  (lambda (F . Lists)
    (if (null? (first Lists)) ()
        (cons (apply F (firsts Lists))
              (apply map (cons F (rests Lists))))))))

(define for-each
 (letrec ((firsts (lambda (L) (if (null? L) () (cons (first (first L)) (firsts (rest L))))))
          (rests  (lambda (L) (if (null? L) () (cons (rest  (first L)) (rests  (rest L)))))))
  (lambda (F . Lists)
    (if (null? (first Lists)) ()
        (begin
           (apply F (firsts Lists)) 
           (apply for-each (cons F (rests Lists))))))))

(define (force x)  (if (procedure? x) (.apply x ()) x))

(define (call/cc k)
  (tryCatch (k (lambda(x) (throw x))) (lambda(x) x)))

;; (define (eval L) (Scheme.eval L Environment.GLOBAL$)) ;;;;; USES jsint eval!

;;  (// "========== SECTION 6.10 INPUT AND OUPUT ==========")
(define (call-with-input-file filename proc)
  (Procedure.tryCatch (lambda()
    (let ((in (open-input-file filename))
          (result (.apply proc (list filename))))
    (begin
       (if (not (eq? in null)) (.close in))
       result)))))

(define (call-with-output-file filename proc)
  (Procedure.tryCatch (lambda()
    (let ((in (open-output-file filename))
          (result (.apply proc (list filename))))
    (begin
       (if (not (eq? in null)) (.close in))
       result)))))


(define (input-port? x) (.isInstance InputPort.class x))
(define (output-port? x) (.isInstance java.io.PrintWriter.class x))
(define (current-input-port) (Scheme.getInput))
(define (current-output-port) (Scheme.getOutput))
;;  (// "Inessential with-input-from-file, with-output-to-file not implemented")

(define (open-input-file filename)
  (Procedure.tryCatch
    (lambda() (InputPort. (java.io.FileInputStream. (.toString filename))))
    (lambda(e) (.println java.lang.System.out$ (list "IOException" e)) null)))

(define (open-output-file filename)
  (Procedure.tryCatch
    (lambda() (java.io.PrintWriter. (java.io.FileWriter. (.toString filename))))
    (lambda(e) (.println java.lang.System.out$ (list "IOException" e)) null)))

(define (close-input-port x) (.close x))
(define (close-output-port x) (begin (.close x) #t ))

(define read (lambda F
  (.read (if (null? F) (Scheme.getInput) F))))
(define read-char (lambda F
  (.readChar (if (null? F) (Scheme.getInput) F))))
(define peek-char (lambda F
  (.peekChar (if (null? F) (Scheme.getInput) F))))
(define (eof-object? x) (eq? x InputPort.EOF$))

;;  (// "Inessential char-ready?, transcript-on, transcript-off not implemented")

(define write (lambda (x . F)
  (let ((port (if (null? F) (Scheme.getOutput) F)))
  (begin
    (.print port (U.stringify x #t))
    (.flush port)
    x))))

(define display (lambda (x . F)
  (let ((port (if (null? F) (Scheme.getOutput) F)))
  (begin
    (.print port (U.stringify x #f))
    (.flush port)
    x))))

(define newline (lambda  F
  (let ((port (if (null? F) (Scheme.getOutput) F)))
  (begin
    (.println port)
    (.flush port)
    #t))))

(define write-char (lambda (x . F)
  (let ((port (if (null? F) (Scheme.getOutput) F)))
  (begin
    (.print port x)
    (.flush port)
    x))))

(define load Scheme.load)

;;  (// "========== EXTENSIONS ==========")
(define (set-procedure-name! x y) (begin (.setName x y) x))

(define macroexpand Macro.expand)

(define error (lambda L (E.error "" L)))
(define (class C) (if (.isInstance Class.class C) C (Import.classNamed (.toString C))))
;(define (import S) (begin (Import.addImport (.toString S)) #t))

(define constructor (lambda (x . L)
  (RawConstructor. (Invoke.findConstructor x L))))
(define method (lambda (x y . L)
  (RawMethod. (Invoke.findMethod (.toString x) y L))))
(define new (lambda (x . L) 
  (Invoke.invokeConstructor (.getName (class x)) (list->vector L))))
(define invoke (lambda (x y . L)
  (Invoke.invokeInstance x y (list->vector L))))
(define invoke-static (lambda (x y . L)
  (Invoke.invokeStatic (class x) y (list->vector L))))
(define (peek x y) (Invoke.peek x (.toString y)))
(define (peek-static x y) (Invoke.peekStatic (class x) (.toString y)))
(define (poke x y z) (Invoke.poke x (.toString y) z))
(define (poke-static x y z) (Invoke.pokeStatic x (.toString y) z))
(define exit (lambda L
  (java.lang.System.exit (if (null? L) 0 (first L)))))

(define time-call
 (letrec ((iter (lambda (N P) (if (<= N 1) (P) (begin (P) (iter (- N 1) P))))))
  (lambda (F N)
    (let* ((runtime (java.lang.Runtime.getRuntime))
           (startTime (begin (.gc runtime) (java.lang.System.currentTimeMillis)))
           (startMem (.freeMemory runtime))
           (ans (iter N F))
           (time (- (java.lang.System.currentTimeMillis) startTime))
           (mem (- startMem (.freeMemory runtime))))
       (list ans (list time 'msec) (list mem 'bytes))))))

(define safe-time-call (lambda (F N)
  (let ((ans1 (time-call (lambda() #t) N))
        (ans2 (time-call F N)))
    (list (first ans2) (list (- (caadr ans2) (caadr ans1)) 'msec) (list (- (caaddr ans1) (caaddr ans2)) 'bytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further Extenstions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Java Scalar Operations
(define %    Op.mod)
(define &    Op.and)
(define ^    Op.xor)
(define |    Op.or)
(define ~    Op.complement)        ;; I need a complement operator by itself
(define <<   Op.leftShift)
(define >>   Op.rightShift)
(define >>>  Op.rightShiftZ)
(define ==   Op.eqv) ;; scalar equality
(define !=   Op.ne) ;; number disequality


;; Java scalar classes
(define (byte? x) (.isInstance java.lang.Byte.class x))
(define (short? x) (.isInstance java.lang.Short.class x))
(define (int? x) (.isInstance java.lang.Integer.class x))
(define (long? x) (.isInstance java.lang.Long.class x))
(define (float? x) (.isInstance java.lang.Float.class x))
(define (double? x) (.isInstance java.lang.Double.class x))

;; list operations (null? null)==> false!
(define (empty? N) (eq? N ()))
(define (java-null? N) (eq? N null))

(define (list->array ComponentType L)
  (define A  (java.lang.reflect.Array.newInstance ComponentType (length L)))
  (define (store L I)
     (if (eq? L ()) A
        (begin
          (java.lang.reflect.Array.set A I (.first$ L))
          (store (.rest$ L) (+ I 1)))))
  (store L 0))


(define (array->list A)
  (define (make-list I L)
   (begin
    (if (< I 0) L
        (make-list (- I 1) 
            (Pair. (java.lang.reflect.Array.get A I) L)))))
  (make-list (- (java.lang.reflect.Array.getLength A) 1) ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; here we have the extensions to R4RS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-macro (time exp . rest)
 `(safe-time-call (lambda () ,exp) . ,rest))

(define (missing-classes classes sofar)
  (if (null? classes) sofar
      (missing-classes (cdr classes)
		       (if (eq? (class (car classes)) #null)
			   (cons (car classes) sofar)
			   sofar))))

(define-macro (if-classes classes then else)
  (if (null? (missing-classes classes '()))
      then
      else))

(define-macro (when-classes classes . then)
  `(if-classes ,classes (begin ,@then) #f))

(define (define-method-runtime name type-names f name-args)
  (let ((missing (missing-classes type-names '())))
    (if (null? missing)
	(jsint.Generic.defineMethod name type-names f)
	(jsint.E.warn (string-append "Can't define-method " name-args
			       " classes " missing " do not exist.")))))
(define-macro (define-method name-args . body)
    (define (arg-name x) (if (pair? x) (car x) x))
    (define (maybe-second x default)
      (if (and (pair? x) (pair? (cdr x))) (cadr x)
	  default))
    (define (arg-type x) (maybe-second x 'java.lang.Object))
    (let* ((name (car name-args))
	   (args (cdr name-args))
	   (arg-types (map arg-type args)))
      `(define-method-runtime
       ',name ',arg-types (lambda ,(map arg-name args) ,@body)
       ',name-args)))
      
(define-macro (package . args) #t)

(define (array a-class . args)
  (let ((v (make-array a-class (length args))))
    (let loop ((i 0)
	       (as args))
      (if (null? as) v
	  (begin
	    (vector-set! v i (car as))
	    (loop (+ i 1) (cdr as)))))))

(define (make-array a-class size)
  (java.lang.reflect.Array.newInstance a-class size))

(define (!{} . args)
  (let loop
      ((args args)
       (sb (StringBuffer.)))
    (cond ((null? args) (.toString sb))
	  ((pair? (car args))
	   (loop (cons (car (car args))
		       (cons (cdr (car args)) (cdr args))) sb))
	  ((null? (car args)) (loop (cdr args) sb))
	  (else (.append sb (U.stringify (car args) #f))
		(loop (cdr args) sb)))))

;;; KRA 05JAN04: This may not work in all Java environments, Netscape
;;; for example.  Also,use this with care Java really expects strings
;;; to be immutable.
(define (string-set! s i v)
  (.hash$# s 0)
  (vector-set! (.value$# s) i v))

(define (string-fill! s x)
  (.hash$# s 0)
  (let ((L (string-length s))
	(v (.value$# s)))
    (let loop ((i 0))
      (if (< i L)
	  (begin (vector-set! v i x)
		 (loop (+ i 1)))))
    s))

(define (string-copy s) (.toString (StringBuffer. s)))



;; general form is some prefix of
;; (use-module FILENAME SPECIFIER SYMBOLS PREFIX)
;; where
;;    FILENAME is a filename, URL, or resource name
;;    SPECIFIER is one of 'import-procedures, 'import-macros, 'import  --- default is import-procedures
;;    SYMBOLS is either 'all or a list of symbols '(a b c ...) -- default is 'all
;;    PREFIX is a prefix string -- default is ""
;; 
(define use-module (lambda (filename . R)
  (case (length R)
    ((0) (use-module filename 'import 'all #f))
    ((1) (use-module filename (first R) 'all #f))
    ((2) (use-module filename (first R) (second R) #f))
    (else 
     (let* ((specifier (first R))
            (symbols (second R))
            (prefix (third R))
            (symarray 
             (if (or (equal? symbols #null) (equal? symbols 'all))
                  #null
                 (list->array jsint.Symbol.class symbols))))
     (case specifier
       ((import-procedures)
         (.environmentImport (Scheme.currentEvaluator) filename prefix #f symarray))
       ((import-macros)
         (.environmentImport (Scheme.currentEvaluator) filename #f #t symarray))
       ((import)
         (.environmentImport (Scheme.currentEvaluator) filename prefix #f symarray)
         (.environmentImport (Scheme.currentEvaluator) filename #f  #t symarray))
       (else (error {unknown specifier [specifier] in (use-module [filename] [specifier] [symbols] [prefix])\n}))))))))

