;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Compiler.scm
;;
;; /**
;;  * @author Timothy J. Hickey, Copyright 2000, tim@cs.brandeis.edu, <a href="license.txt">license</a>
;;  * subsequently modified by Jscheme project members
;;  * Copyright (c) 2000 Ken R. Anderson, Tim Hickey, Peter Norvig
;;  * licensed under zlib licence (see license.txt)
;;  *<a href="license.txt">license</a>
;;  */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "java.lang.*")
(import "jsint.*")
(import "java.io.*")
(import "java.util.*")
(import "java.lang.reflect.*")

;; create vectors to hold compiled subterms
(define  Quotes (java.util.Vector.))
(define  Javadefs (java.util.Vector.))
(define  Imports (java.util.Vector.))
(define  Lambdas (java.util.Vector.))
(define  Setglobals (java.util.Vector.))
(define  PrimsUsed (java.util.Vector.))
(define  Macros (java.util.Hashtable.))
(define  Globals  (Array.newInstance java.lang.Object.class 10))
(define ClassName 0)
(define PackageName 1)
(define UseReflection 2)
(define Verbose 3)
(Array.set Globals ClassName ())
(Array.set Globals PackageName ())
(Array.set Globals UseReflection #f)
(Array.set Globals Verbose #f)

(tryCatch 
   (jsint.compiler.Reflect.load) 
   (lambda(e) 
      #t))

(tryCatch 
   (jsint.compiler.CompileReflection.load) 
   (lambda(e) 
       #t))

    (define (reset-compiler)
          (.removeAllElements Quotes)
          (.removeAllElements Javadefs)
          (.removeAllElements Imports)
          (.removeAllElements Lambdas)
          (.removeAllElements Setglobals)
          (.removeAllElements PrimsUsed)
          (.clear Macros)
         )


(define compile-file
(let ((compile  (lambda (fullfilename)
  (let ((PROG (let readlist 
                   ((L (open-input-file fullfilename)))
                      (let ((a (read L)))
                         (if (eof-object? a) ()
                             (cons a (readlist L))))))
          (PATH_CLASSNAME 
              (let (
                   (filesep File.separator$)
                   (AbsPath (.getAbsolutePath (java.io.File. fullfilename))))
                 (let* (
                    (lastfilesep (.lastIndexOf AbsPath filesep))
                    (lastdot (.lastIndexOf AbsPath "."))
                    (PATH (.substring AbsPath 0 (+ 1 lastfilesep)))
                    (CLASSNAME (.substring AbsPath (+ 1 lastfilesep) lastdot)))
                   (list PATH CLASSNAME)))))
      (Array.set Globals ClassName (second PATH_CLASSNAME))

      (if (Array.get Globals Verbose) (begin (display "first pass of compiler")  (newline)) "")
      (let ((a (map (lambda (x) (compileExpr x () #f)) PROG)))
         (if (Array.get Globals Verbose) (begin (display (list "compiling to Java")) (newline)) "")
         (let* ((CODE (GENCODEtoplevel a))
                (OUTPORT (open-output-file (string-append (first PATH_CLASSNAME)  (second PATH_CLASSNAME) ".java"))))
            (if (Array.get Globals Verbose) (begin (display (list "writing to " PATH_CLASSNAME)) (newline)) "")
            (printCode OUTPORT CODE)
            (close-output-port OUTPORT)
            (if (Array.get Globals Verbose) (begin (display (list "compilation complete")) ) "")
            (newline)
          ))))))
(lambda R
 (reset-compiler)
 (if (> (length R) 1) (Array.set Globals PackageName (second R)))
 (compile (first R)))))



(define (compileExpr Expr ENV LCall)
  (letrec (
	   ;; (lookupSymbol S Env) returns
	   ;; 'javaliteral if S is a Java literal
	   ;; 'globalvalue of S is not in Env
	   ;; (i x) if S is in pos x of the ith frame L of Env (0-based)
	   ;;   and x codes for both elements and tails of L
	   ;;   the elements are in positions 1,3,5,..
	   ;;   and the tails are 0,2,4...
	   (lookupSymbol
	    (lambda (S Env)
	      (letrec (
		       (isJavaLiteral (lambda (Sym) (> (.indexOf (.toString Sym) ".") -1)))  
		       (listPosition  (lambda (S L P) 
					(cond ((null? L) -1)
					      ((not (pair? L)) 
					       (if (equal? L S) P -1))
					      ((equal? (first L) S) (+ P 1))
					      (else
					       (listPosition S (cdr L) (+ P 2)))))))
		(cond ((isJavaLiteral S) 'javaliteral)
		      ((null? Env) 'globalsymbol)
		      (else
		       (let ((INDEX (listPosition S (first Env) 0)))
			 (if (>=  INDEX 0)
			     (list 0 INDEX)
			     (let ((pos (lookupSymbol S (rest Env))))
			       (if (symbol? pos) 
				   pos
				   (list (+ 1 (first pos)) (second pos)))))))))))

	   (compileQuote GENCODEquoteref)

	   (compileIf 
	    (lambda (Test Then ElseList ENV LCall)
	      (let ((TESTCODE (compileExpr Test ENV #f))
		    (THENCODE (compileExpr Then ENV LCall))
		    (ELSECODE (if (null? ElseList) (compileExpr #t ENV LCall) (compileExpr (car ElseList) ENV LCall))))
		(GENCODEif TESTCODE THENCODE ELSECODE))))
    
	   (compileApplication 
	    (lambda (fn args env LCall)
	      (let ((FUNCODE (compileExpr fn env #f))
		    (ARGSCODE (map (lambda (x) (compileExpr x env #f)) args)))
		(apply GENCODEapplication  (cons LCall (cons FUNCODE ARGSCODE))))))
    
    
	   (compileSymbol 
	    (lambda (S ENV) 
	      (let ((POSITION (lookupSymbol S ENV)))
		(cond ((equal? POSITION 'globalsymbol)
		       (GENCODEgetglobal S))
		      ((equal? POSITION 'javaliteral)
		       (GENCODEgetjava S))
		      (else 
		       (GENCODEgetlocal POSITION))))))

	   (compileLambda 
	    (lambda (args body ENV)
	      (define (compileBody exprs env)
		(letrec (
			 (extractDefines 
			  (lambda (Defines Body)
			    (if (or (null? Body) (not (pair? (car Body)))
				    (not (equal? 'define (caar Body))))
				(list Defines Body)
				(let* ((x (first Body))
				       (z (second x))
				       (d (if (not (pair? z))  x 
					      `(define ,(first z) (lambda ,(rest z) ,@(rest (rest x)))))))
				  (extractDefines (cons d Defines) (rest Body))))))
			 (compileExprSequence 
			  (lambda (body env)
			    (if (null? body) ()
				(cons (compileExpr (car body) env (null? (cdr body)))
				      (compileExprSequence (cdr body) env))))))
  
		  (let* ((TEMP (extractDefines () exprs ))
			 (SIMPLEDEFINES (reverse (first TEMP)))
			 (NEWBODY (second TEMP)))
		    (if (null? SIMPLEDEFINES)
			(compileExprSequence exprs env)
			(list (compileExpr `(letrec ,(map rest SIMPLEDEFINES) ,@NEWBODY) env #t))))))

	      (GENCODElambda 
	       (compileBody body (cons args ENV)))))
    
	   (compileAssignment 
	    (lambda (defOrSet head body ENV)
	      (define (compileVarAssignment defOrSet defvar defbody ENV)
		(if (Array.get Globals Verbose) (begin (display (list "compiling " defvar)) (newline)) "")
		(let ((BODY (compileExpr defbody ENV #f))
		      (POS (lookupSymbol defvar ENV)))
		  (cond ((equal? POS 'globalsymbol)
			 (GENCODEsetglobal defvar BODY))
			((equal? POS 'javaliteral) 
			 (GENCODEsetjava defvar BODY))
			(else
			 (GENCODEsetlocal POS BODY)))))
	      (if (pair? head)
		  (compileVarAssignment 
		   defOrSet (car head) (append (list 'lambda (cdr head)) body) ENV)
		  (compileVarAssignment
		   defOrSet head       (car body)                              ENV))))

	   (compileImport 
	    (lambda (x) 
	      (if (.contains Imports x) 
		  (compileExpr (list 'quote x) () #f)
		  (begin
		    (.addElement Imports x)
		    (addImport x)
		    (GENCODEaddimport x)))))
    
	   (compilePackage 
	    (lambda (args)
	      (begin (Array.set Globals PackageName (symbol->string (first args))) " ")))


	   (expandBegin 
	    (lambda (args) `((lambda() ,@args))))

	   (expandAnd 
	    (lambda (args)  
	      (if (null? args) 
		  #t
		  `(if ,(first args) (and ,@(rest args)) #f))))
    
	   (expandOr ;  (or a b ...) --> ((lambda (x) (if x x ((lambda (y) (if y y ...)) b) a) but the
	    (lambda (args)  
	      (if (null? args) 
		  #f
                  `((lambda(x) (if x x ,(expandOr (rest args)))) ,(first args)))))

	   (expandCond 
	    (letrec (
		     (iter 
		      (lambda (args)
			(if (null? args) '()
			    (let ((TEST (caar args))
				  (THEN (cdar args)) 
				  (REST (cdr args)))
			      `(if ,(if (eq? TEST 'else) #t TEST)
				   ,(if (= 1 (length THEN)) (car THEN) (cons 'begin THEN))
				   ,(iter REST)))))))
	      iter))
    

	   (expandLet 
	    (lambda (args)
	      (let ((BINDINGS (first args)) 
		    (BODY (rest args)))
		(if (symbol? BINDINGS)
		    (let named-let ((name BINDINGS) (bindings (first BODY)) (body (rest BODY)))
		      `(let ((,name #f))
			 (set! ,name (lambda ,(map first bindings) ,@body))
			 (,name ,@(map second bindings))))
		    `((lambda  ,(map first BINDINGS) ,@BODY) ,@(map second BINDINGS))))))
    
	   (expandLet* 
	    (lambda (args)
	      (let ((BINDINGS (first args)) 
		    (BODY (rest args)))
		(if (null? BINDINGS) 
		    (if (= 1 (length BODY)) (first BODY) `(begin ,@BODY))
		    `((lambda  (,(first (first BINDINGS))) (let* ,(cdr BINDINGS) ,@BODY)) ,(second (first BINDINGS)))))))
    
	   (expandLetrec 
	    (lambda (args)
	      (let ((BINDINGS (first args)) 
		    (BODY (rest args)))
		`(let  ,(map (lambda (x) (list x #f)) (map first BINDINGS)) 
		   ,@(map (lambda (var val) (list 'set! var val)) (map first BINDINGS) (map second BINDINGS))
		   ,@BODY))))
    
	   (expandCase
	    (lambda (args)
	      (let ((exp (first args)) (cases (rest args)))
		(let ((do-case (lambda (case)
				 (if (eq? (first case) 'else) case
				     `((member <exp> ',(first case)) ,@(rest case))))))
		  `(let ((<exp> ,exp)) (cond ,@(map do-case cases)))))))

	   (expandDo
	    (lambda (args)
	      (let ((bindings (first args)) (test-and-result (second args)) (body (rest(rest args))))
		(let ((variables (map first bindings))
		      (inits (map second bindings))
		      (steps 
		       (map 
			(lambda (clause)
			  (if (null? (cddr clause))
			      (first clause)   
			      (third clause)))
			bindings)))
		  `(letrec ((<loop>
			     (lambda ,variables
			       (if ,(first test-and-result)
				   (begin  ,@(rest test-and-result))
				   (begin 
				     ,@body
				     (<loop>  ,@steps))))))
		     (<loop>  ,@inits))))))

	   (expandDelay
	    (lambda (args) 
	      (let ((exp (first args)))
		(define (make-promise proc)
		  (let ((result-ready? #f)
			(result #f))
		    (lambda ()
		      (if result-ready?
			  result
			  (let ((x (proc)))
			    (if result-ready?
				result
				(begin 
				  (set! result-ready? #t)
				  (set! result x)
				  result)))))))
		`(,make-promise (lambda () ,exp)))))
  
	   (expandTryCatch 
	    (lambda (args)
	      (list 'jsint.Procedure.tryCatch (list 'lambda () (first args)) (second args))))

	   (expandDefineMethod
	    (lambda (args)
	      (let ((name-args (first args)) (body (rest args)))
		(define (arg-name x) (if (pair? x) (car x) x))
		(define (maybe-second x default)
		  (if (and (pair? x) (pair? (cdr x))) (cadr x)
		      default))
		(define (arg-type x) (maybe-second x 'Object))
		(let ((name (car name-args))
		      (args (cdr name-args)))
		  `(jsint.Generic.defineMethod 
		    ',name
		    ',(map arg-type args)
		    (lambda ,(map arg-name args) ,@body))))))

	   (expandMacro
	    (lambda (args)
	      `(jscheme.REPL.eval '(define-macro ,@args))))
    
	   (expandQQ 
	    (lambda (exp)
	      ;; The quasiquote code is from Darius Bacon <djello@well.com>
	      ;; who started with Peter Norvig's PAIP code and modified it.
	      (define (constant? exp)
		(if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
	      (define (combine-skeletons left right exp)
		(cond
		 ((and (constant? left) (constant? right)) 
		  (if (and (eqv? (eval left) (car exp))
			   (eqv? (eval right) (cdr exp)))
		      (list 'quote exp)
		      (list 'quote (cons (eval left) (eval right)))))
		 ((null? right) (list 'list left))
		 ((and (pair? right) (eq? (car right) 'list))
		  (cons 'list (cons left (cdr right))))
		 (else (list 'cons left right))))
	      (define (expand-quasiquote exp nesting)
		(cond
		 ((vector? exp)
		  (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
		 ((not (pair? exp)) 
		  (if (constant? exp) exp (list 'quote exp)))
		 ((and (eq? (car exp) 'unquote) (= (length exp) 2))
		  (if (= nesting 0)
		      (second exp)
		      (combine-skeletons ''unquote 
					 (expand-quasiquote (cdr exp) (- nesting 1))
					 exp)))
		 ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
		  (combine-skeletons ''quasiquote 
				     (expand-quasiquote (cdr exp) (+ nesting 1))
				     exp))
		 ((and (pair? (car exp))
		       (eq? (caar exp) 'unquote-splicing)
		       (= (length (car exp)) 2))
		  (if (= nesting 0)
		      (list 'append (second (first exp))
			    (expand-quasiquote (cdr exp) nesting))
		      (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
					 (expand-quasiquote (cdr exp) nesting)
					 exp)))
		 (else (combine-skeletons (expand-quasiquote (car exp) nesting)
					  (expand-quasiquote (cdr exp) nesting)
					  exp))))
	      (expand-quasiquote exp 0)))



	   )
    (cond 
     ((symbol?   Expr)  (compileSymbol  Expr ENV))
     ((boolean?  Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((char?     Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((number?   Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((string?   Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((null?     Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((eq? #null Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     ((vector?   Expr)  (compileExpr `(quote ,Expr) ENV LCall))
     (else 
      (if (not (pair? Expr))  (display (list "ERROR: can not compile " Expr))
	  (let ((fn (car Expr)) (args (cdr Expr)))
	    (cond
	     ((.containsKey Macros fn)
	      (compileExpr (apply (.get Macros fn) args) ENV LCall))
	     ((eq? fn 'quote)     (compileQuote (first args)))
	     ((eq? fn 'if)        (compileIf (first args) (second args) (cddr args) ENV LCall))
	     ((eq? fn 'lambda)    (compileLambda (car args) (cdr args) ENV))  
	     ((eq? fn 'define)    (compileAssignment 'define   (car args) (cdr args) ENV))
	     ((eq? fn 'set!)      (compileAssignment 'set!     (car args) (cdr args) ENV))
	     ((eq? fn 'import)    (compileImport (first args)))
	     ((eq? fn 'package)   (compilePackage args))
	     ((eq? fn 'begin)     (compileExpr (expandBegin args) ENV LCall))  
	     ((eq? fn 'and)       (compileExpr (expandAnd args) ENV LCall))
	     ((eq? fn 'or)        (compileExpr (expandOr args) ENV LCall))
	     ((eq? fn 'cond)      (compileExpr (expandCond args) ENV LCall))     
	     ((eq? fn 'let)       (compileExpr (expandLet args) ENV LCall))
	     ((eq? fn 'let*)      (compileExpr (expandLet* args) ENV LCall))
	     ((eq? fn 'letrec)    (compileExpr (expandLetrec args) ENV LCall))
	     ((eq? fn 'case)      (compileExpr (expandCase args) ENV LCall))     
	     ((eq? fn 'do)        (compileExpr (expandDo args) ENV LCall))     
	     ((eq? fn 'delay)     (compileExpr (expandDelay args) ENV LCall))     
	     ((eq? fn 'quasiquote)(compileExpr (expandQQ (first args)) ENV LCall))
	     ((eq? fn 'tryCatch)  (compileExpr (expandTryCatch args) ENV LCall))
	     ((eq? fn 'define-method)
	      (compileExpr (expandDefineMethod args) ENV LCall))
	     ((eq? fn 'define-macro)
	      (.put Macros (caar args) (eval `(lambda ,(cdar args) ,@(cdr args))))
	      (compileExpr (expandMacro args) ENV LCall)
	      )
	     (else                (compileApplication fn args ENV LCall)))))))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print the code to a file 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (printCode F X)
    (define (printCodeList F X)
      (printCode F (car X))
      (if (null? (cdr X)) ()
          (printCodeList F (cdr X))))

    (cond ((pair? X)
           (printCodeList F X))
          ((null? X) 
           ())
          ((equal? X #null)
           "null")
          (else
           (.print F X))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation to Java
;;
;;  The next section consists of (GENCODExxxx ....)
;;  procedures which generate Java code corresponding
;;  to particular types of Scheme expressions.
;;
;;  All Java specific code is in this section.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convertNameToJava S)
    (define chars 
       (if (symbol? S) 
           (string->list (symbol->string S))
           (string->list S)))
    (define (convertChar X)
       (cond ((or (char-alphabetic? X) (char-numeric? X)) (list->string (list X)))
             (else (string-append "_" (number->string (char->integer X)) "_"))))
    (if (isKeyword S)
        (string->symbol (apply string-append (cons "_" (map convertChar chars))))
        (string->symbol (apply string-append (map convertChar chars)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (GENCODEgetlocal  LOCATION) 
  (let ((LEVEL (first LOCATION)) (POS (second LOCATION)))
    (if (equal? LEVEL 0)
      `(  "(( Pair) Args).getEltNover2("   ,POS ")"   )
      `(  "(((Pair) ((this.frame).nth(" ,(- LEVEL 1) " ))).getEltNover2("   ,POS "))"   ))))


(define (GENCODEsetlocal   LOCATION BODY) 
  (let ((LEVEL (first LOCATION)) (POS (second LOCATION)))
    (if (equal? LEVEL 0)
      `(  "(( Pair) Args).setEltNover2("   ,POS "," ,BODY ")"   )
      `(  "(((Pair) ((this.frame).nth(" ,(- LEVEL 1) " ))).setEltNover2("   ,POS "," ,BODY"))"   ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables (i.e., 
;;   toplevel defined variables and Scheme prims)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (GENCODEgetglobal  NAME) 
   (if (and 
            (not (.contains Setglobals NAME))
            (not (.contains PrimsUsed (.toString NAME))))
       (.addElement PrimsUsed (.toString NAME)))
  `("dynEnv.getValue((Symbol)" ,(convertNameToJava NAME) ")")
)

(define  (GENCODEdefglobal S)
  `("public static final Object " ,(convertNameToJava S) " = Symbol.intern(\"" ,S "\");\n\n"))


;; globals are set by simple assignment
(define (GENCODEsetglobal  DEFVAR BODY)  
   (if (and (not (.contains Setglobals DEFVAR))
            (not (.contains PrimsUsed (.toString DEFVAR))))
       (.addElement Setglobals DEFVAR))
  `("dynEnv.setValue(Symbol.intern(\"" ,DEFVAR "\"), " ,BODY ")" ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (GENCODElambda  BODYCODE)
  (let ((N (let ((s (.size Lambdas)))
              (begin (.addElement Lambdas (list s BODYCODE)) 
                     s))))
    `("new " ,(Array.get Globals ClassName) "(USER_DEF, " ,N ", new Pair( Args, this.frame), dynEnv)" )))

(define (GENCODElambdaref N) 
  ` (_L ,N))

(define  (GENCODElambdadef L)
  `(" Object " ,(GENCODElambdaref (first L)) "(Pair Args){\n   Object tmp=null; \n " 
       ,(map (lambda(x) (list "tmp = " x ";\n\n")) (second L))
      "\n   return tmp; \n}\n\n")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if, application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (GENCODEif   TEST THEN ELSE)
 `("\n  (Boolean.FALSE.equals(" ,TEST ")\n  ?\n  " ,ELSE "\n  :\n  " ,THEN "\n  )" ))
; `("\n  (((Boolean)" ,TEST ").booleanValue()\n  ?\n  " ,THEN "\n  :\n  " ,ELSE "\n  )" ))



(define GENCODEapplication  
 (lambda (LastCall FUN . ARGS)
    (define (makelist As)
          (if (null? As) 
              "Pair.EMPTY"
              `("new Pair(" ,(first As) ",\n    "  ,(makelist (rest As)) ")" )))
    (if LastCall
       `("new LCO(" ,FUN "," ,(makelist ARGS) ")" )
       `("((jsint.Procedure) " ,FUN ").apply(" ,(makelist ARGS) ")" ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (GENCODEaddimport  PACKAGE)
 (begin
   `("addImport(\"" ,PACKAGE "\")")))

(define (GENCODEimport x)
  `("import " ,x ";\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define  (GENCODEquotedef Q)
  `("   static Object " ,(GENCODEquoteref Q) "="  ,(GENquote  Q) ";\n\n")
)

;; quoted terms are static java vars
(define (GENCODEquoteref x) 
  (let ((quoteIndex 
          (begin
             (if (not (.contains Quotes x))
                 (.addElement Quotes x))
             (.indexOf Quotes x))))
  ` ("_C" ,quoteIndex)))


(define (GENquote TERM)
    (define (byte? x) (.isInstance Byte.class x))
    (define (short? x) (.isInstance Short.class x))
    (define (int? x) (.isInstance Integer.class x))
    (define (long? x) (.isInstance Long.class x))
    (define (float? x) (.isInstance Float.class x))
    (define (double? x) (.isInstance Double.class x))
    (cond 
        ((null? TERM) "Pair.EMPTY")
        ((pair? TERM) 
            (if (and (= (length TERM) 3) (.equals "." (second TERM))) 
                `(" new Pair(" ,(GENquote (first TERM)) ",\n  " ,(GENquote (third TERM)) ") ")
                `("new Pair(" ,(GENquote (first TERM)) ","  ,(GENquote (rest TERM)) ")" )))
        ((boolean? TERM) (if TERM '("Boolean.TRUE") '("Boolean.FALSE")))
        ((char?   TERM) `("new Character('" ,TERM "')" ))   
        ((byte?   TERM) `("new      Byte(" ,(.byteValue   TERM) ")" ))    
        ((short?  TERM) `("new     Short(" ,(.shortValue  TERM) ")" ))    
        ((int?    TERM) `("new   Integer(" ,(.intValue    TERM) ")" ))    
        ((long?   TERM) `("new      Long(" ,(.longValue   TERM) ")" ))    
        ((float?  TERM) `("new     Float(" ,(.floatValue  TERM) ")" ))    
        ((double? TERM) `("new    Double(" ,(.doubleValue TERM) ")" ))    
        ((string? TERM)  `( ,(jsint.U.stringify TERM) ))
        ((eq? TERM #null) '("null"))   ;; the first null must not be quoted
        ((symbol? TERM)  `( "Symbol.intern( ",(jsint.U.stringify (symbol->string TERM)) ")"))
        ((vector?  TERM) `( "new Object[]{ " 
                              ,(map (lambda (x) (list (GENquote x) ",\n")) (vector->list TERM)) "}"))
        (else (display `(ERROR in GENquote ,TERM)) ' 'ERROR))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme Primitives -- preloaded into the Symbol table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define  (GENCODEprimdef S) 
  `("   static Object " ,(convertNameToJava S)
         "= Symbol.intern(\"" ,(.toString S) "\");\n\n"))

;; (let ((X (.get primitives S)))
;;    `("   static Object " ,(convertNameToJava S)
;;         "= new jsint.Primitive(\""  ,(.toString S) "\"," 
;;         ,(first X) "," ,(second X) "," ,(third X) ");\n\n" )
;;  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; references to java class literals are themselves
(define (GENclass name)
  (.replace
    (.concat (.toString name) ".class")
    #'$' #'.'))


(define (GENCODEjavaref S) 
    (define (storeJavadef S)
         (if (not (.contains Javadefs S))
             (begin
               (.addElement Javadefs S)
               (- (.size Javadefs) 1))
           (.indexOf Javadefs S)))
  (string-append "_J" (.toString (storeJavadef S))
                 "/* " (.toString S) " */"
))


(define (GENCODEgetjava S)
 (let ((isJavaStaticField  
         (and
           (!=  (.indexOf (.toString S) ".") 0)
           (= (.lastIndexOf (.toString S) "$") (- (.length (.toString S)) 1)))))
   (if isJavaStaticField
      (list "(((jsint.Procedure)"
            (GENCODEjavaref S)
            ").apply(jsint.Pair.EMPTY))")
      (GENCODEjavaref S))))


(define (GENCODEsetjava S Args)
  (if (Array.get Globals UseReflection)
        (list      
          "((JavaField) "
          (GENCODEjavaref S)
          ").apply(new Object[]{ new Pair("
          Args
          ",Pair.EMPTY)})")

        (list      
          "((jsint.Procedure)"
          (GENCODEjavaref S)
          ").apply( new Pair(" Args ",Pair.EMPTY))")))


(define  (GENCODEjavadef J)
  (letrec (  ;; GENCODEjavadef
   (GENjava
    (lambda (Type . Args)
        (cond  
           ((equal? Type 'instancefield)
            (if (equal? (second Args) "")
               `("new JavaField(" ,(GENquote (first Args)) ", null)")
               `("new JavaField(" ,(GENquote (first Args)) "," ,(GENclass (second Args)) ")")))
    
           ((equal? Type 'staticfield)
               `("new JavaField(" ,(GENquote (first Args)) "," ,(GENclass (second Args)) ")"))
    
           ((equal? Type 'instancemethod)
            (if (equal? (second Args) "")
              `("new JavaMethod(" ,(GENquote (first Args)) ", null, false)")
              `("new JavaMethod(" ,(GENquote (first Args)) ", " ,(GENclass (second Args)) ", false)")))
    
           ((equal? Type 'staticmethod)
              `("new JavaMethod(" ,(GENquote (first Args)) "," ,(GENclass (second Args)) ", true)"))
    
           ((equal? Type 'constructor)
              `("new JavaConstructor(" ,(GENclass (first Args)) ")"))
    
           ((equal? Type 'class) 
               (GENclass (first Args)))
    
           (else (string-append "ERROR in Java Interface" (.toString Type) (.toString Args))))))
   (compileJavaLiteral
     (lambda (S)
         (let* ((name (.toString S))
                (firstIndex (.indexOf name "."))
                (lastIndex (.lastIndexOf name "."))
                (fieldIndex (.lastIndexOf name "$"))
                (nameLength (.length name)))
           (cond 
                 ((= fieldIndex (- nameLength 1))
                  (if (= firstIndex 0) 
                    `( instancefield                                             ;; .Class.Field$
                       ,(.substring name (+ 1 lastIndex) (- nameLength 1))
                       ,(.substring name (min 1 lastIndex) lastIndex))
                    `( staticfield                                                ;;  Class.Field$
                       ,(.substring name (+ 1 lastIndex) (- nameLength 1))
                       ,(.substring name                 0 lastIndex))      ))
                 ((= firstIndex 0)                                                
                 `( instancemethod                                                ;; .Class.Method
                       ,(.substring name (+ 1 lastIndex)  nameLength)
                       ,(.substring name (min 1 lastIndex) lastIndex)
                        ))
                 ((= lastIndex (- nameLength 1))                                  ;; Class.
                 `( constructor ,(.substring name 0 lastIndex)))
                 ((> firstIndex 0)                                                
                   (let ((classname (.substring name 0 lastIndex))
                         (methodname (.substring name (+ 1 lastIndex) nameLength)))
                      (if (.equals "class" methodname)
                         `( class ,classname)                                     ;; Class.class
                         `( staticmethod ,methodname ,classname))))               ;; Class.Method
                  (else                                                           ;; VAR
                     (display (list "Error in compileJavaliteral" S name firstIndex lastIndex fieldIndex nameLength))
                 `(ERROR ERROR)))))))
     (let ((specifier (compileJavaLiteral J)))
        (if (Array.get Globals Verbose) (begin (display (list "compiling" specifier J)) (newline)) "")
        (if (and 
              (not (equal? 'class (first specifier)))
              (not (Array.get Globals UseReflection))
              (or (< (length specifier) 3) (> (.length (third specifier)) 0)))
              (begin
                 (list
                   (compileGeneric specifier (GENCODEjavaref J))
                  `("static Object " 
                   ,(GENCODEjavaref J) 
                   " = new " 
                  ,(Array.get Globals ClassName)
                   "( JAVA_LIT, " 
                   ,(.indexOf Javadefs J)
                   "/* " ,J "*/"
                   ", Pair.EMPTY)"
                   ";\n\n")))

            `(
              "public static Object " ,(GENCODEjavaref J) "(Pair Args){\n"
               "  return (((jsint.Procedure)"
                 ,(GENCODEjavaref J) 
                 ").apply(Args));}\n"   
              "static Object " 
              ,(GENCODEjavaref J) 
              " = "  
             ,(apply GENjava (compileJavaLiteral J)) 
             ";\n\n")))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toplevel -- putting it all together
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; KRA 07MAR02: Original version.  This overflows the stack in JDK 1.4.
;;; Replaced it with a tail recursive version.
;'(define makecases
; (lambda (n F max)
;   (if (>= n max) 
;       " "
;       `("          case " ,n ": return(" ,(F n) "(args)); \n"
;	 ,@(makecases (+ n 1) F max)))))

(define (GENCODEtoplevel  TOPLEVEL)
  (begin  
    (if (Array.get Globals Verbose) (begin (display (list "starting GENCODEtoplevel")) (newline)) "")
    (letrec ((makecases
	      (lambda (n F max)
		;; (print `(makecases ,n ,F ,max))
		(let loop ((n n)
			   (sofar '()))
		  (if (>= n max) 
		      (apply append (reverse sofar))
		      (loop (+ n 1) (cons 
				     `("          case " ,n ": return(" ,(F n) "(args)); \n")
				     sofar))))))
             (vectorToList 
	      (lambda (V L)
                (let ((pos (- (.size V) 1)))
                  (if (equal? pos -1) L
		      (let ((x (.lastElement V)))
                        (begin
                          (.removeElementAt V pos)
                          (vectorToList V (cons x L))))))))
             (vectorToNumList 
	      (lambda (N V L)
                (let ((pos (- (.size V) 1)))
                  (if (equal? pos -1) L
		      (let ((x (.lastElement V)))
                        (begin
                          (.removeElementAt V pos)
                          (vectorToNumList (+ N 1) V (cons (list pos x) L)))))))))
      (let (
	    (PACKAGE (Array.get Globals PackageName))
	    (CLASSNAME (Array.get Globals ClassName))
	    (Cs (makecases 0 GENCODElambdaref (.size Lambdas)))
	    (Ds (makecases 0 (lambda(n) `("_J" ,n))  (.size Javadefs)))
	    (TOPLEVEL_CODE (map (lambda(x) (list  x ";\n\n")) TOPLEVEL))

	    (IMPORTS (map GENCODEimport       (vectorToList Imports ())))
	    (QUOTE_DEFS (map GENCODEquotedef     (vectorToList Quotes ())))
	    (JAVALITERAL_DEFS (map GENCODEjavadef      (vectorToList Javadefs ())))
	    (GLOBALVAR_DEFS (map GENCODEdefglobal (vectorToList Setglobals ())))
	    (LAMBDA_DEFS (map GENCODElambdadef   (vectorToList Lambdas ())))
	    (SCHEMEPRIM_DEFS (map GENCODEprimdef      (vectorToList PrimsUsed ())))
	    )

	(list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  START -- PACKAGE SPECIFICATION

	 (if (.isInstance String.class PACKAGE) (list "package " PACKAGE) "// no package name")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FILEHEADER
	 ";
  /**
    * this file is automatically generated by the jscheme->javac compiler Compiler.scm. 
    * Modify at your own risk!
    */
  import jsint.*;
  import java.lang.reflect.*;
  import java.util.*;
  "

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPORTS

	 IMPORTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CLASSHEADER
	 "
  
  public class "  CLASSNAME " extends jsint.Procedure implements jsint.Function,  Runnable {  
      public int whichcode=0;  // corresponds to a numbering of the toplevel procedures of the program
      public int whichtype=0;  // 0 = user defined procedure, 1 = java literal
      public static final int USER_DEF=0, JAVA_LIT=1;
      public Pair frame;
      public DynamicEnvironment dynEnv;

      public " CLASSNAME "() {
          super();
          dynEnv=Scheme.getInteractionEnvironment();
      }
      public " CLASSNAME "(int t, int n, Pair f) {
          whichtype = t;
          whichcode = n;
          frame = f;
          dynEnv = Scheme.getInteractionEnvironment();
      }
      public " CLASSNAME "(int t, int n, Pair f, DynamicEnvironment d) {
          whichtype = t;
          whichcode = n;
          frame = f;
          dynEnv = d;
      }
  
      private Boolean addImport(String s) {
         jsint.Import.addImport(s);
         return Boolean.TRUE;
      }
  
      public void run() {
        this.invoke(null);
      } 
  
      public Object[] makeArgArray(jsint.Pair args) {
         return new Object[]{args};
      }

      public Object apply(Object[] args) {
      return invoke((Pair)args[0]);
      }
  
      public Object apply(Pair args) {
        return invoke(args);
      }
  
      public Object invoke(Pair args)
      {
        return LCO.eval(invoke1(args));
      }
  
    static Object tmp;
  
  
    public static void load() { 
      new " CLASSNAME "().init();
    }

    public static void load(String shellArgs[]) {
       Scheme.getInteractionEnvironment().setValue(Symbol.intern(\"shellArgs\"), shellArgs);
       load();
    }

    public static void main(String shellArgs[]) {
       Symbol main = jsint.Symbol.intern(\"main\");
       load(shellArgs);
       if (main.isDefined())
          {
             DynamicEnvironment dynEnv = Scheme.getInteractionEnvironment();
             ((jsint.Procedure) (dynEnv.getValue(main))).apply(new Pair(shellArgs,Pair.EMPTY));
          }
     }
"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN SWITCH
	 "
      public Object invoke1(Pair args) {
      if (whichtype == USER_DEF) {
       switch (whichcode) {
"
	 Cs

	 "
         default:  break;
       }}
      else {
       switch (whichcode) {
"
	 Ds

	 "
         default:  break;
       }}



      return null;
      }

"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INITPROCEDURE
	 "
  
   public void init() {
     Pair Args = null;
     dynEnv.setValue(Symbol.intern(\"this\"), this);
     Class _p = Primitive.class; // this loads the primitives
"
	 TOPLEVEL_CODE

	 "  
   }
"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 "\n // definitions of global variables \n"
	 GLOBALVAR_DEFS
	 "\n // definitions of Scheme variables defined externally\n"
	 SCHEMEPRIM_DEFS
	 "\n // definitions of quoted terms \n"
	 QUOTE_DEFS
	 "\n // definitions of embedded lambdas \n"
	 LAMBDA_DEFS 
	 "\n // definitions of java literals\n"
	 JAVALITERAL_DEFS

	 "
    //static { new " CLASSNAME "().init();}
    }
"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END
	 )))))


(define keywords (Vector.))
(for-each (lambda (x) (.addElement keywords x))
 '(abstract boolean break byte case catch char class const continue default do
  double else extends final finally float for goto if implements import instanceof
  int interface long native new null package private protected public return short
  static super switch synchronized this throw throws transient try void volatile while))

(define (isKeyword x)
  (if (.isInstance Symbol.class x)
      (.contains keywords x)
      (.contains keywords (Symbol.intern (.toString x)))))

