;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here I am trying to get the compiler to work as an interpreter
;; by just redefining the GENCODE procedures.....
;; This is not a trivial task but it would be nice to be
;; able to just maintain one front end and two back ends...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toplevel procedures -- the read-eval-print loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define  Macros (java.util.Hashtable.))
(define  Globals  (Array.newInstance java.lang.Object.class 10))
(define ClassName 0)
(define PackageName 1)
(define UseReflection 2)
(define Verbose 3)
(define init1
 (begin 
   (Array.set Globals ClassName ())
   (Array.set Globals PackageName ())
   (Array.set Globals UseReflection #f)
   (Array.set Globals Verbose #f)
   3))					; KRA 28APR02: Why return 3?

(define (start)
  (REPloop () (create-global-environment)))

(define (REPloop ENV-VARS ENV-VALS)
  (print "\njscheme >> ")   
  (let ((G (read)))
    (if (equal? G '(exit)) #t
    (if (equal? G '(quit)) #f
    ((compileExpr G ENV-VARS #f) ENV-VALS 
         (lambda(k) (print "\n **ERROR** --> ") (print k) (REPloop ENV-VARS ENV-VALS))
         (lambda(k) (print "\n --> ") (print k) (REPloop ENV-VARS ENV-VALS)))))))


(define (interpretExpr Expr)
  ((compileExpr Expr () #f) (create-global-environment) (top-failure-cont) (top-success-cont) ))

(define (interpretProg Prog)
  ((compileProg Prog) (create-global-environment) (top-failure-cont) (top-success-cont) ))

(define (compileProg Prog)
  (apply GENCODEsequence (compileExprs Prog () #f)))

(define (compileExprs Exprs env LCall)
  (map (lambda (x) (display (list 'compiling x))(newline) (compileExpr x env LCall)) Exprs))

(define (top-success-cont) (lambda (k) (display (list 'done  k))(newline) k))
(define (top-failure-cont) (lambda (k) (display (list 'error k))(newline) k))

(define (interpret-file fullfilename)
  (define PROG (readlist (open-input-file fullfilename)))
  (interpretProg PROG))

(define (readlist L)
  (let ((a (read L)))
    (if (eof-object? a) ()
        (cons a (readlist L)))))





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
			(apply GENCODEsequence (compileExprSequence exprs env))
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

;	   (compileImport 
;	    (lambda (x) 
;	      (if (.contains Imports x) 
;		  (compileExpr (list 'quote x) () #f)
;		  (begin
;		    (.addElement Imports x)
;		    (jsint.Import.addImport x) ;; TJH change 4/27/02
;		    (GENCODEaddimport x)))))

           (compileTryCatch 
             (lambda (args ENV LCall)
               (let ((BODYCODE (compileExpr (first args) ENV LCall))
                     (EXCEPTIONCODE (compileExpr (second args) ENV LCall)))
                 (GENCODEtryCatch BODYCODE EXCEPTIONCODE))))

           (compileThrow 
             (lambda (args ENV LCall)
               (let ((BODYCODE (compileExpr (first args) ENV LCall)))
                 (GENCODEthrow BODYCODE))))

          (compileCallCC 
             (lambda (args ENV LCall)  
               (let ((BODYCODE (compileExpr (first args) ENV LCall)))
                  (GENCODEcallcc BODYCODE))))


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
    
	   (expandOr 
	    (lambda (args)  
	      (if (null? args) 
		  #f
		  `(if ,(first args) #t (or ,@(rest args))))))
    
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
  
;	   (expandTryCatch 
;	    (lambda (args)
;	      (list 'jsint.Procedure.tryCatch 
;                (list 'lambda () (first args)) (second args))))

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

          (expandImport 
             (lambda (args)
                `(jsint.Import.addImport ,(first args))))

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
;	     ((eq? fn 'import)    (compileImport (first args)))
	     ((eq? fn 'tryCatch)  (compileTryCatch args ENV LCall))
	     ((eq? fn 'call/cc)   (compileCallCC  args ENV LCall))
	     ((eq? fn 'throw)     (compileThrow args ENV LCall))
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
;	     ((eq? fn 'tryCatch)  (compileExpr (expandTryCatch args) ENV LCall))
	     ((eq? fn 'import)    (compileExpr (expandImport args) ENV LCall))
	     ((eq? fn 'define-method)
	      (compileExpr (expandDefineMethod args) ENV LCall))
	     ((eq? fn 'define-macro)
	      (.put Macros (caar args) (eval `(lambda ,(cdar args) ,@(cdr args))))
	      (compileExpr (expandMacro args) ENV LCall)
	      )
	     (else                (compileApplication fn args ENV LCall)))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (compileSymbol S ENV) 
     (let ((POSITION (lookupSymbol S ENV))) 
         (if (null? POSITION)
             (compileGlobalSymbol S)
             (GENCODEgetlocal POSITION))))

  (define (compileGlobalSymbol S)
    (if (isJavaSymbol S)
       (if (isStaticJavaField? S)
         (GENCODEstaticjava S)
         (GENCODEjava S))  ;;; this is new
       (GENCODEgetglobal S)))

  (define (isStaticJavaField? Sym)
    (let ((Str (.toString Sym)))
      (and (not (= (.indexOf Str ".") 0))  
          (= (.indexOf Str "$") (- (.length Str) 1)))))


  (define (isJavaSymbol Sym)
    (let ((Str (.toString Sym)))
      (or (> (.indexOf Str ".") -1)  
          (> (.indexOf Str "$") -1))))



  (define (lookupSymbol S Env)
     (if (null? Env) ()
     (let ((INDEX (listPosition S (first Env))))
        (if (< INDEX 0)
            (let ((pos (lookupSymbol S (rest Env))))
                 (if (null? pos) 
                     ()
                     (list (+ 1 (first pos)) (second pos))))
            (list 0 INDEX)))))


       ;; x=0 (x)=1 (a.x) = 2 (a x)=3 (a b . x)=4 (a b x)=5 ...
  (define (listPosition S L)
    (if (null? L) 
        -1
        (if (equal? L S) 
          0
          (if (not (pair? L))
           -1
            (if (equal? (first L) S)
              1
              (let ((pos (listPosition S (cdr L))))
                 (if (< pos 0) 
                     pos 
                     (+ 2 pos))))))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENCODE
;; Here is where we actually compile syntax into code
;;
;;    (GENCODEgetglobal 'variablename)
;;    (GENCODEgetlocal '(depth pos))
;;    (GENCODEquote      'TERM)
;;    (GENCODEsetglobal     'variablename CODE)
;;    (GENCODEsetlocal      '(depth pos) CODE)
;;    (GENCODEapplication   CODE ... CODE)
;;    (GENCODElambda     CODE)
;;    (GENCODEif            CODE CODE CODE)
;;    (GENCODEsequence    CODE ... CODE)
;;    (GENCODEstaticjava Symbol)                    ;; generic access to Java Static Fields
;;    (GENCODEjava Symbol)                          ;; generic access to Java libraries
;;    (GENCODEimport PACKAGE)                       ;; and Java packages
;;    (GENCODEcallcc BODYLAMBDA)                    
;;    (GENCODEtryCatch BODY EXCEPTIONLAMBDA)
;;    (GENCODEthrow BODYEXPR)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; these are the three new GENCODEs needed to implement full call/cc and tryCatch/throw

(define (GENCODEcallcc BODY)
  (lambda (F FC SC) (BODY F FC (lambda(k) (k SC)))))

(define (GENCODEtryCatch BODY ELSE)
  (lambda (F FC SC) (BODY F (lambda (e) (ELSE F FC (lambda(k) (SC (k e))))) SC)))

(define (GENCODEthrow BODY)
  (lambda (F FC SC) (BODY F FC (lambda(k) (FC k)))))



;; Local variables

(define (GENCODEgetlocal  LOCATION) 
      (lambda (F0 FC SC) 
        (SC (getLocal LOCATION F0))))

(define (GENCODEsetlocal  POS BODY)
    (lambda (F1 SC FC) 
      (BODY F1 
            FC 
           (lambda(k) 
              (tryCatch 
                   (SC (setLocal POS k F1)) 
                   (lambda(e) (FC e)))))))


;; Global variables

(define (GENCODEgetglobal  NAME) 
     (let ((G (getGlobal NAME)))
        G))

(define (GENCODEsetglobal  DEFVAR BODY)  
  (if (isStaticJavaField? DEFVAR)
      (let ((proc (compileJavaSymbol DEFVAR)))
         (lambda(F3 FC SC) 
            (BODY F3 FC (lambda(k) 
              (tryCatch (SC (proc k)) (lambda(e) (FC e)))))))
      (lambda (F4 FC SC) 
           (BODY F4 FC (lambda(k) 
              (tryCatch (SC (setGlobal DEFVAR k F4)) (lambda(e) (FC e))))))))

(define (GENCODEgetjava S)
  (compileGlobalSymbol S))

;; lambda

(define (GENCODElambda  BODY)
    (lambda (F5 FC SC)
     (SC
       (lambda g
         (BODY (cons (cons g (car F5)) (cdr F5)) 
               FC 
              (lambda(k) k))))))


;; if, application

(define (GENCODEif  TEST THEN ELSE)
    (lambda (F6 FC SC)
      (TEST F6 FC (lambda(k)
        (if k 
            (THEN F6 FC SC)
            (ELSE F6 FC SC))))))

;;; used to define GENCODEapplication
(define (doCPS ARGS vals F7 FC SC)
       (if (null? ARGS) 
           (let ((appl (reverse vals)))
              (tryCatch 
                  (SC (apply (first appl) (rest appl)))
                  (lambda(e) 
                    (display (list "errors " e))(newline) (FC e)))
              )
           ((first ARGS) F7 FC (lambda(k) 
                (doCPS (rest ARGS) (cons k vals) F7 FC SC)))))

(define GENCODEapplication
  (lambda (LastCall FUN . ARGS)
    (case (length ARGS)
      ((0)
         (lambda (Fapp FC SC)
            (FUN Fapp FC (lambda(kf) (SC (kf))))))
      ((1)
         (let ((A0(first ARGS)))
           (lambda (Fapp FC SC)
              (FUN Fapp FC (lambda(kf) 
                (A0 Fapp FC (lambda(a0)
                   (SC (kf a0)))))))))
      ((2)
         (let ((A0 (first ARGS))
               (A1 (second ARGS)))
           (lambda (Fapp FC SC)
              (FUN Fapp FC (lambda(kf) 
                (A0 Fapp FC (lambda(a0)
                (A1 Fapp FC (lambda(a1)
                   (SC (kf a0 a1)))))))))))

      (else
        (lambda (Fapp FC SC)
          (FUN Fapp FC (lambda (kf) (doCPS ARGS (list kf) Fapp FC SC))))))))
          

;;  imports 
(define GENCODEimport (lambda R (display "import not yet implemented\n")))
(define GENCODEaddimport (lambda R (display "addimport not yet implemented\n")))


;; quote

(define (GENCODEquoteref  TERM) 
    (lambda (F8 FC SC) 
        (SC TERM)))


;;; used to define GENCODEsequence
(define (doseqCPS ARGS lastval F9 FC SC)
       (if (null? ARGS) 
           (SC lastval)
           ((first ARGS) F9 FC (lambda(k) 
                (doseqCPS (rest ARGS) k F9 FC SC)))))

(define GENCODEsequence
  (lambda ARGS
    (case (length ARGS)
     ((0) 
       (lambda(F10 FC SC)
         (SC #f)))
     ((1) 
       (let ((A0 (first ARGS)))
         A0))
     ((2) 
       (let ((A0 (first ARGS))
             (A1 (second ARGS)))
         (lambda(F10 FC SC)
            (A0 F10 FC (lambda(k1)
            (A1 F10 FC SC))))))
     ((3) 
       (let ((A0 (first ARGS))
             (A1 (second ARGS))
             (A2 (third ARGS)))
         (lambda(F10 FC SC)
            (A0 F10 FC (lambda(k1)
            (A1 F10 FC (lambda(k2)
            (A2 F10 FC SC))))))))
     (else
      (lambda(F10 FC SC)
        (doseqCPS ARGS #f F10 FC SC))))))


;(define GENCODEsequence
;  (lambda ARGS
;    (lambda(F10 FC SC)
;      (doseqCPS ARGS #f F10 FC SC))))



(define (GENCODEstaticjava S)
  (let ((proc (compileJavaSymbol S)))
     (lambda(F11 FC SC) (SC (proc)))))

(define (GENCODEjava S)
  (let ((Reflector (compileJavaSymbol S)))
    (lambda (F12 FC SC) (SC Reflector))))

;; This compiles Java Reflector symbols into Scheme procedures
;; for calling those Java members

(define (compileJavaSymbol S)
     (let* ((name (.toString S))
            (firstIndex (.indexOf name "."))
            (lastIndex (.lastIndexOf name "."))
            (fieldIndex (.lastIndexOf name "$"))
            (nameLength (.length name)))
       (cond 
             ((= fieldIndex (- nameLength 1))                                            ;; Field syntax
               (if (= firstIndex 0)               
                 (if (= firstIndex lastIndex)
                     (JavaField. (.substring name 1 fieldIndex) #null)                    ;; .INSTANCEFIELD$
                     (JavaField. (.substring name 1 lastIndex)                           ;; .CLASS.INSTANCEFIELD$
                             (Import.classNamed (.substring name (+ 1 lastIndex) fieldIndex))))
                 (JavaField. (.substring name (+ 1 lastIndex) fieldIndex)                ;; CLASS.STATICFIELD$
                             (Import.classNamed (.substring name 0 lastIndex)))))

             ((= lastIndex 0)                                                           ;; .INSTANCEMETHOD
               (JavaMethod. (.substring name 1 nameLength) #null))

             ((= lastIndex (- nameLength 1))                                            ;; CONSTRUCTOR.
               (JavaConstructor. (Import.classNamed (.substring name 0 lastIndex))))

             ((> lastIndex 0)                                                           
               (let ((classname (.substring name 0 lastIndex))
                     (methodname (.substring name (+ 1 lastIndex) nameLength)))
                  (if (.equals "class" methodname)
                     (Import.classNamed classname)                                      ;; CLASS.class
                     (JavaMethod. methodname(Import.classNamed classname)))))           ;; CLASS.STATICMETHOD
             (else                                                                      ;; VAR
             `(ERROR ERROR)))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RUNTIME SYSTEM
;;
;; The Environment actually consists of two parts
;; a local frame (list of lists) and a global env (hashtable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (getEnv x hashtable)  
  (if (.containsKey hashtable x)
      (.get hashtable x)
      (.concat "unbound identifier: " (.toString x))))


(define (setEnv x y hashtable) (.put hashtable x y))


;; NOTE: here we are hardwiring values defined in the global environment!
(define (getGlobal Symbol)
  (if (.containsKey (cdr the-global-environment) Symbol)
      (let ((val (.get (cdr the-global-environment) Symbol)))
          (lambda(F13 FC SC) (SC val)))
      (lambda(F14 FC SC) (SC (getEnv Symbol (cdr F14))))))

(define (setGlobal Symbol Value Env) 
  (setEnv Symbol Value (cdr Env)))
;; NOTE: We could use the Symbol class to store global values!


(define (getLocal Pos Frame) ;;
  (getIndex (second Pos) (nth (first Pos) (car Frame)))
)

(define (setLocal Pos Val Frame) ;;
  (if (equal? (second Pos) 0)
      (setnth (first Pos) (car Frame) Val)
      (setIndex (second Pos) Val (nth (first Pos) (car Frame)))))

(define (nth N L) ;;
  (if (null? L)
      ()
      (if (equal? N 0)
          (car L)
          (if (< N 0)
              ()
              (nth (- N 1) (cdr L))))))
        



(define (setnth N V L) ;;
  (if (equal? N 0)
      (set-car! L V)
      (setnth (- N 1) (cdr L))))
        

(define (getIndex I L) ;;
  (if (vector? L) (vector-ref L (/ I 2))
      (if (equal? I 0)
           L
          (if (equal? I 1)
              (first L)
              (getIndex (- I 2) (cdr L))))))


(define (setIndex I V L) ;;
  (if (vector? L) (vector-set! L (/ I 2) V)
      (if (equal? I 1)   
          (set-car! L V)
          (if (equal? I 2) 
              (set-cdr! L V)
              (setIndex (- I 2) V (cdr L))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating the Global Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;; Create global Environment which is a hashtable at compile-time
  ;; at runtime all global references are hardwired into the code.
  ;; We will replace this with an array at some point.


;;; here we define the scheme primitive environment

(define schemeprims `(
    (time-call ,time-call)
    (exit ,exit)
    (poke-static ,poke-static)
    (poke ,poke)
    (peek-static ,peek-static)
    (peek ,peek)
    (invoke-static ,invoke-static)
    (invoke ,invoke)
    (new ,new)
    (method ,method)
    (constructor ,constructor)
    (import ,import)
    (class ,class)
    (error ,error)
    (macroexpand ,macroexpand)
    (set-procedure-name! ,set-procedure-name!)
;    (load ,load)
    (load ,interpret-file)
    (write-char ,write-char)
    (newline ,newline)
    (display ,display)
    (write ,write)
    (eof-object? ,eof-object?)
    (peek-char ,peek-char)
    (read-char ,read-char)
    (read ,read)
    (close-output-port ,close-output-port)
    (close-input-port ,close-input-port)
    (open-output-file ,open-output-file)
    (open-input-file ,open-input-file)
    (current-output-port ,current-output-port)
    (current-input-port ,current-input-port)
    (output-port? ,output-port?)
    (input-port? ,input-port?)
    (call-with-output-file ,call-with-output-file)
    (call-with-input-file ,call-with-input-file)
;    (eval ,eval)
    (eval ,interpretExpr)
;    (call-with-current-continuation ,call-with-current-continuation)
;    (call/cc ,call/cc)
    (force ,force)
    (for-each ,for-each)
    (map ,map)
    (apply ,apply)
    (procedure? ,procedure?)
    (list->vector ,list->vector)
    (vector->list ,vector->list)
    (vector-set! ,vector-set!)
    (vector-ref ,vector-ref)
    (vector-length ,vector-length)
    (vector ,vector)
    (make-vector ,make-vector)
    (vector? ,vector?)
    (list->string ,list->string)
    (string->list ,string->list)
    (string-append ,string-append)
    (substring ,substring)
    (string-ci<=? ,string-ci<=?)
    (string-ci>=? ,string-ci>=?)
    (string-ci>? ,string-ci>?)
    (string-ci<? ,string-ci<?)
    (string<=? ,string<=?)
    (string>=? ,string>=?)
    (string>? ,string>?)
    (string<? ,string<?)
    (string-ci=? ,string-ci=?)
    (string=? ,string=?)
    (string-set! ,string-set!)
    (string-ref ,string-ref)
    (string-length ,string-length)
    (string ,string)
    (make-string ,make-string)
    (string? ,string?)
    (char-downcase ,char-downcase)
    (char-upcase ,char-upcase)
    (integer->char ,integer->char)
    (char->integer ,char->integer)
    (char-lower-case? ,char-lower-case?)
    (char-upper-case? ,char-upper-case?)
    (char-whitespace? ,char-whitespace?)
    (char-numeric? ,char-numeric?)
    (char-alphabetic? ,char-alphabetic?)
    (char-ci<=? ,char-ci<=?)
    (char-ci>=? ,char-ci>=?)
    (char-ci>? ,char-ci>?)
    (char-ci<? ,char-ci<?)
    (char-ci=? ,char-ci=?)
    (char<=? ,char<=?)
    (char>=? ,char>=?)
    (char>? ,char>?)
    (char<? ,char<?)
    (char=? ,char=?)
    (char? ,char?)
    (string->number ,string->number)
    (number->string ,number->string)
    (inexact->exact ,inexact->exact)
    (exact->inexact ,exact->inexact)
    (expt ,expt)
    (sqrt ,sqrt)
    (atan ,atan)
    (acos ,acos)
    (asin ,asin)
    (tan ,tan)
    (cos ,cos)
    (sin ,sin)
    (log ,log)
    (exp ,exp)
    (round ,round)
    (truncate ,truncate)
    (ceiling ,ceiling)
    (floor ,floor)
    (lcm ,lcm)
    (gcd ,gcd)
    (modulo ,modulo)
    (remainder ,remainder)
    (quotient ,quotient)
    (abs ,abs)
    (/ ,/)
    (- ,-)
    (* ,*)
    (+ ,+)
    (min ,min)
    (max ,max)
    (even? ,even?)
    (odd? ,odd?)
    (negative? ,negative?)
    (positive? ,positive?)
    (zero? ,zero?)
    (>= ,>=)
    (<= ,<=)
    (> ,>)
    (< ,<)
    (= ,=)
    (inexact? ,inexact?)
    (exact? ,exact?)
    (integer? ,integer?)
    (rational? ,rational?)
    (real? ,real?)
    (complex? ,complex?)
    (number? ,number?)
    (string->symbol ,string->symbol)
    (symbol->string ,symbol->string)
    (symbol? ,symbol?)
    (assoc ,assoc)
    (assv ,assv)
    (assq ,assq)
    (member ,member)
    (memv ,memv)
    (memq ,memq)
    (list-ref ,list-ref)
    (list-tail ,list-tail)
    (reverse ,reverse)
    (append ,append)
    (length ,length)
    (list ,list)
    (list? ,list?)
    (null? ,null?)
    (cddddr ,cddddr)
    (cdddar ,cdddar)
    (cddadr ,cddadr)
    (cddaar ,cddaar)
    (cdaddr ,cdaddr)
    (cdadar ,cdadar)
    (cdaadr ,cdaadr)
    (cdaaar ,cdaaar)
    (cadddr ,cadddr)
    (caddar ,caddar)
    (cadadr ,cadadr)
    (cadaar ,cadaar)
    (caaddr ,caaddr)
    (caadar ,caadar)
    (caaadr ,caaadr)
    (caaaar ,caaaar)
    (cdddr ,cdddr)
    (cddar ,cddar)
    (cdadr ,cdadr)
    (cdaar ,cdaar)
    (cadar ,cadar)
    (caadr ,caadr)
    (caaar ,caaar)
    (cddr ,cddr)
    (cdar ,cdar)
    (caar ,caar)
    (third ,third)
    (caddr ,caddr)
    (second ,second)
    (cadr ,cadr)
    (set-cdr! ,set-cdr!)
    (set-car! ,set-car!)
    (rest ,rest)
    (cdr ,cdr)
    (first ,first)
    (car ,car)
    (cons ,cons)
    (pair? ,pair?)
    (equal? ,equal?)
    (eq? ,eq?)
    (eqv? ,eqv?)
    (boolean? ,boolean?)
    (not ,not)
))


(define javaprims `(
  (+   ,Op.add)
  (-   ,Op.sub)
  (*   ,Op.mul)
  (/   ,Op.div)
  (%   ,Op.mod)
  (&   ,Op.and)
  (^   ,Op.xor)
  (|   ,Op.or)
  (~   ,Op.complement)        
  (<<  ,Op.leftShift)
  (>>  ,Op.rightShift)
  (>>> ,Op.rightShiftZ)
  (=   ,Op.eq)
  (<   ,Op.lt)
  (>   ,Op.gt)
  (<=  ,Op.le)
  (>=  ,Op.ge)
  (!=  ,Op.ne)
  (load ,interpret-file)
  (eval ,interpretExpr)
))


(define (create-global-environment)
  (define hashtable (Hashtable.))
  (define (put x y) (.put hashtable x y))
;  (for-each     (lambda (x) (put (first x) (second x)))    javaprims)
  (for-each     (lambda (x) (put (first x) (second x)))    schemeprims)
  (cons () hashtable))

(define the-global-environment (create-global-environment))


(define (print x) (display x))
(define (println x) (display x) (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting the interpreter at load time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zz (println "starting up!"))
(define resume (start))

