(begin

(set! null #null) 

;;;;;;;;;;;;;;;; 4.2 DERIVED EXPRESSION TYPES
;; Defined in order needed to define the next ones, not in R4RS order.
;;; (define a b) => (set! a b); 
;;; (define (p x) y z) => (set! p (set-procedure-name! (lambda (x) y z) 'p))
(set! define
  (set-procedure-name!
   (macro (var . body)
    (if (pair? var)
        (list 'set! (first var)
              (list 'set-procedure-name!
                    (cons 'lambda (cons (rest var) body))
		    (list 'quote (first var))))
        (cons 'set! (cons var body))))
   'define))

(define cond
  (set-procedure-name!
   (macro clauses
     (define (process-clause clause else-part)
       (if (not (pair? clause));; atom
	   (error '(bad cond clause:) clause)
	   (if (null? (rest clause));; (test)
	       (list 'or (first clause) else-part)
	       (if (eq? (second clause) '=>);; (test => proc)
		   ((lambda (tempvar)
		      (list (list 'lambda (list tempvar)
				  (list 'if
					tempvar
					(list (third clause) tempvar)
					else-part))
			    (first clause)))
		    (string->symbol "temp var"))
		   (if (member (first clause) '(#t else));; (else x y z)
		       (cons 'begin (rest clause))
		       (list 'if (first clause) (cons 'begin (rest clause)) 
			     else-part))))))
     ;; body of cond
     (if (null? clauses)
	 #f
	 (process-clause (first clauses) (cons 'cond (rest clauses)))))
   'cond))

(define tryCatch
  (set-procedure-name!
   (macro args
     (list 'jsint.Procedure.tryCatch
	   (list 'lambda () (first args))
	   (second args)))
   'tryCatch))

(define and
  (set-procedure-name!
   (macro args 
     (cond ((null? args) #t)
	   ((null? (rest args)) (list 'U.and1 (first args)))
	   (else (list 'if (first args) (cons 'and (rest args)) #f))))
   'and))

;; OR is treated in Scheme.eval(), not here.

;; Rationale: the translation to Scheme is complex: (or a b c) =>
;; ((lambda (x) (if x x ((lambda (y) (if y y c)) b) a) but the
;; implementation in Scheme.eval is easier. Also note that if OR is
;; treated as primitive, then COND can expand to OR.  The quasiquote,
;; and a few others, are from Darius Bacon <djello@well.com> (But
;; then, he started with my PAIP code, and modified it.)
(define quasiquote
  (set-procedure-name!
   (macro (x) 
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
     (expand-quasiquote x 0))
   'quasiquote))

(define let
  (set-procedure-name!
   (macro (bindings . body)
     (define (varval v) (string->symbol (string-append v "=")))
     (define (named-let name bindings body)
       ;; KRA 08JAN04: (let ((f -)) (let f ((n (f 1))) n)) return 1 not -1.
;       `(let ((,name #f))
;	  (set! ,name (lambda ,(map first bindings) . ,body))
;	  (,name . ,(map second bindings))))
       ((lambda (new-bindings)		; Can't use let yet!
	 `(let ,(cons `(,name #f) new-bindings)
	    (set! ,name (lambda ,(map first bindings) . ,body))
	    (,name . ,(map car  new-bindings))))
	(map (lambda (b) `(,(varval (car b)) ,(cadr b))) bindings)))
     (if (symbol? bindings) 
	 (named-let bindings (first body) (rest body))
	 `((lambda ,(map first bindings) . ,body) . ,(map second bindings))))
   'let))

(define let*
  (set-procedure-name!
   (macro (bindings . body)
     ;; KRA 08JAN04: (let* () (define x 8) x) did not expand properly.
;;     (if (null? bindings) `(begin . ,body)
     (if (null? bindings) (jsint.Scheme.toBody body)
	 (if (null? (cdr bindings))
	     `(let (,(first bindings)) . ,body)
	     `(let (,(first bindings))
		(let* ,(rest bindings) . ,body)))))
   'let*))

(define letrec
  (set-procedure-name!
   (macro (bindings . body)
     (let ((vars (map first bindings))
	   (vals (map second bindings)))
       `(let ,(map (lambda (var) `(,var #f)) vars)
	  ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
	  ;; KRA 08JAN04: (letrec ((x 3)) (define x 10) x) blew out.
	  ;; . ,body)))
	  ,(jsint.Scheme.toBody body))))
   'letrec))
    

(define case
  (set-procedure-name!
   (macro (exp . cases)
     (let ((tempvar (string->symbol "temp var")))
       (define (do-case case)
	 (cond ((not (pair? case)) (error '(bad syntax in case:) case))
	       ((eq? (first case) 'else) case)
	       (else `((member ,tempvar ',(first case)) . ,(rest case)))))
       `(let ((,tempvar ,exp)) (cond . ,(map do-case cases)))))
   'case))

(define do
  (set-procedure-name!
   (macro (bindings test-and-result . body)
     (let ((variables (map first bindings))
	   (inits (map second bindings))
	   (steps (map (lambda (clause)
			 (if (null? (cddr clause))
			     (first clause)   
			     (third clause)))
		       bindings))
	   (result (if (null? (cdr test-and-result)) ''unspecified
		       `(begin . ,(cdr test-and-result)))))
       (let ((tempvar '<loop>))
	 `(letrec ((,tempvar
		    (lambda ,variables
		      (if ,(first test-and-result)
			  ,result
			  (begin 
			    ,@body
			    (,tempvar . ,steps))))))
	    (,tempvar . ,inits)))))
   'do))

(define delay
  (set-procedure-name!
   (macro (exp) 
     (define (make-promise proc)
       (let ((result-ready? #f)
	     (result #f))
	 (lambda ()
	   (if result-ready?
	       result
	       (let ((x (proc)))
		 (if result-ready?
		     result
		     (begin (set! result-ready? #t)
			    (set! result x)
			    result)))))))
     `(,make-promise (lambda () ,exp)))
   'delay))

;;;;;;;;;;;;;;;; Derived Expression Extensions
(define time
  (set-procedure-name!
   (macro (exp . ntimes)
     `(time-call (lambda () ,exp) ,(if (pair? ntimes) (car ntimes)
				       1)))
   'time))

(define define-macro
  ;; (define-macro (newif Test Then Else) `(if ,Test ,Then ,Else))
  ;; (define-macro when (lambda (Test . Actions) `(if ,Test (begin . ,Actions))))
  (set-procedure-name! 
   (macro (spec . body) 
     (if (pair? spec)
	 `(define ,(first spec)
	    (set-procedure-name! (macro ,(rest spec) . ,body)
				 ',(first spec)))
	 `(define ,spec
	    (set-procedure-name! (macro ,(second (first body))
				   ,@(rest (rest (first body))))
	     ',spec))))
   'define-macro))

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


(define-macro (class-case varlist . clauses)
   (define (runtimeClassName c)
     (string->symbol (string-append (.getName (class c)) ".class")))
   (define (instanceof v c) `(.isInstance ,(runtimeClassName c) ,v))
   `(cond ,@(map (lambda (clause)
                   (if (equal? (first clause) 'else) clause
                       `((and ,@(map instanceof varlist (first clause)))
                         ,@(rest clause))))

                 clauses)))

(define (define-method-runtime name type-names f name-args)
  (let ((missing (missing-classes type-names '())))
    (if (null? missing)
          (jsint.Generic.defineMethod name type-names f)
	(jsint.E.warn (string-append "Can't define-method " name-args
			       " classes " missing " do not exist.")))))
(define define-method
  (macro (name-args . body)
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
       ',name-args))))
      
(define package  (macro args #t))

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
  ;; KRA 21JUN04: This is like (apply string-append (flaten args)).
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

(define !#{} !{})


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
(define use-module
  (lambda (filename . R)
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
           (else (error {unknown specifier [specifier] in (use-module [filename] [specifier] [symbols] [prefix])\n}))))))
    ))
)					; End of begin.
