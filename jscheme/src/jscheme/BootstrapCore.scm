;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BootstrapCore.scm
;;
;; Here we define the CoreJscheme primitives using kernel Jscheme
;; This is a heavily bootstrapped file with each definition depending
;; on the earlier ones. I have added lines of semicolons
;; to indicate that the following section depends on the previous section
;; On the other hand, definitions inside a section can be reordered.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first we replace the current Scheme environment with an empty environment
;; so that the only symbols that are defined are 
;;
;;  * the javadot symbols
;;    constructors, instance/static methods, instance/static fields, classes, and
;;
;;  * the seven internally define special forms
;;     set! lambda macro begin quote if or
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(.interactionEnvironment$ (Scheme.currentEvaluator) (jsint.DynamicEnvironment.))


;; this is a convenience definition so we can write Op.add instead of jsint.Op.add, etc.

(set! import (lambda(S) (begin (jsint.Import.addImport (.toString S)) #t)))
(import "jscheme.*")
(import "jsint.*")

;; Now we define define and define-macro, and
;; to do this we define a few helper procedures first ...

(set! set-procedure-name! 
  (lambda(proc name)
    (.setName proc name) 
     proc))

(set! list (lambda R R))
(set! pair? (lambda(x) (.isInstance jsint.Pair.class x)))
(set! cons jsint.Pair.)
(set! first .first$)
(set! rest .rest$)
(set-procedure-name! list 'list)
(set-procedure-name! pair? 'pair?)

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

(set! define-macro
 (set-procedure-name! 
   (macro (spec . body) 
     (if (pair? spec)
	 (list 'define (first spec)
	    (list 'set-procedure-name! 
                 (cons 'macro (cons (rest spec)  body))
		 (list 'quote (first spec))))
	 (list 'define spec
	    (list 'set-procedure-name! 
                 (cons 'macro (cons (second (first body)) (rest (rest (first body)))))
	     (list 'quote spec)))))
   'define-macro
))
       

(set! set-name! (lambda(name proc) 
  (if (.isInstance Procedure.class proc) (.setName proc name)) proc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some helpful procedures for handling errors:
;;   (error a b c d) returns #null
;;   (throw x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define error (lambda R (.println java.lang.System.out$ (.concat "ERROR:" (.toString R))) #null))
(define (throw x)
   (jsint.Procedure.throwRuntimeException
      (jsint.JschemeThrowable. x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic list processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (null? x) 
   (jsint.Op.sameObject x jsint.Pair.EMPTY$))
(define (not x) (.equals #f x))
(define (pair? p) (if (null? p) #f (.isInstance Pair.class p)))
(define cons Pair.)
(define first .first$)
(define rest .rest$)
(define list (set-name! 'list (lambda R R)))
(define car .first$)
(define cdr .rest$)

; these could be optimized, e.g. 
; cadr=.second     caddr=.third    
; (caadr x)= (car (.second x)), (cadadr x) =(.second (.second x))
;

(define (caar L) (car (car L)))
(define (cadr L) (car (cdr L))) ;; .second
(define (cdar L) (cdr (car L)))
(define (cddr L) (cdr (cdr L))) 

(define (caaar L) (car (car (car L))))
(define (caadr L) (car (car (cdr L))))
(define (cadar L) (car (cdr (car L))))
(define (caddr L) (car (cdr (cdr L)))) ;; .third
(define (cdaar L) (cdr (car (car L))))
(define (cdadr L) (cdr (car (cdr L))))
(define (cddar L) (cdr (cdr (car L))))
(define (cdddr L) (cdr (cdr (cdr L))))

(define (caaaar L) (car (car (car (car L)))))
(define (caaadr L) (car (car (car (cdr L)))))
(define (caadar L) (car (car (cdr (car L)))))
(define (caaddr L) (car (car (cdr (cdr L)))))
(define (cadaar L) (car (cdr (car (car L)))))
(define (cadadr L) (car (cdr (car (cdr L)))))
(define (caddar L) (car (cdr (cdr (car L)))))
(define (cadddr L) (car (cdr (cdr (cdr L)))))
(define (cdaaar L) (cdr (car (car (car L)))))
(define (cdaadr L) (cdr (car (car (cdr L)))))
(define (cdadar L) (cdr (car (cdr (car L)))))
(define (cdaddr L) (cdr (car (cdr (cdr L)))))
(define (cddaar L) (cdr (cdr (car (car L)))))
(define (cddadr L) (cdr (cdr (car (cdr L)))))
(define (cdddar L) (cdr (cdr (cdr (car L)))))
(define (cddddr L) (cdr (cdr (cdr (cdr L)))))


(define second  .second)
(define third   .third)
(define (fourth L)  (first (list-tail L 3)))
(define (fifth L)   (first (list-tail L 4)))
(define (sixth L)   (first (list-tail L 5)))
(define (seventh L) (first (list-tail L 6)))
(define (eighth L)  (first (list-tail L 7)))
(define (ninth L)  (first (list-tail L 8)))
(define (tenth L)  (first (list-tail L 9)))

(define length .length)

(define append (lambda Lists
  (define (appendtwo L R)
    (if (null? L) R (cons (first L) (appendtwo (rest L) R))))
  (define (iter Lists)
    (if (null? Lists)
        ()
      (if (null? (cdr Lists))
          (car Lists)
          (appendtwo (car Lists) (iter (cdr Lists))))))
  (iter Lists)))

(define (reverse L) (.reverse L))

(define (list-tail L N)  (.listTail L N))

(define list-ref .nth)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (boolean? x) (.isInstance java.lang.Boolean.class x))
(define (symbol? S) (.isInstance Symbol.class S))
(define (number? N) (.isInstance java.lang.Number.class N))
(define (string? x) (.isInstance java.lang.String.class x))
(define (vector? v) (.isArray (.getClass v)))
(define (procedure? x) (.isInstance Procedure.class x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean ops  and simple comparisons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (and . args)
    (if (null? args)
         #t
	 (if (null? (rest args)) 
             (first args)
	     (list 'if (first args) (cons 'and (rest args)) #f))))

(define eq? 
   (lambda (x y) 
       (or (Op.sameObject x y) 
           (and (.equals #t x) (.equals #t y))
           (and (.equals #f x) (.equals #f y))))) 

(define eqv? Op.eqv)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (if (eqv? x y)
       #t
    (if (and (pair? x) (pair? y)) 
        (eqpair? x y)
      (if (and (.isArray (.getClass x)) (.isArray (.getClass y)))
          (eqarray? x y)
          #f))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; membership
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memq x L)
  (if (or (null? L) (not(pair? L))) 
      #f
      (if (eq? x (first L))
           L
          (memq x (cdr L)))))
(define (memv x L)
  (if (or (null? L) (not(pair? L))) 
      #f
      (if (eqv? x (first L))
           L
          (memv x (cdr L)))))
(define (member x L)
  (if (or (null? L) (not(pair? L))) 
      #f
      (if (equal? x (first L))
           L
          (member x (cdr L)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (cond . clauses)
    (define (process-clause clause else-part)
      (if (not (pair? clause)) ;; atom
          (error '(bad cond clause:) clause)
          (if (null? (rest clause)) ;; (test)
              (list 'or (first clause) else-part)
              (if (eq? (second clause) '=>) ;; (test => proc)
                  (list (list 'lambda '(<_>) 
                         (list 'if '<_> (list (third clause) '<_>) else-part))
                        (first clause))
                  (if (member (first clause) '(#t else)) ;; (else x y z)
                      (cons 'begin (rest clause))
                      (list 'if (first clause) (cons 'begin (rest clause)) 
                            else-part))))))
    ;; body of cond
    (if (null? clauses)
        #f
        (process-clause (first clauses) (cons 'cond (rest clauses)))))

(define-macro (tryCatch . args) (list 'Procedure.tryCatch (list 'lambda () (first args)) (second args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; eval is defined with respect to the load-time evaluator
; hence it evaluates terms in "this" local environment
; We haven't defined let yet, hence the ugly code...

(define eval 
 ((lambda(js)
     (lambda R (.apply .eval (cons js R)))
    )
  (jsint.Scheme.currentEvaluator)
 )
)
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The quasiquote, and a few others, are from Darius Bacon <djello@well.com>
;; (But then, he started with Peter Norvig's PAIP code, and modified it.)

(define-macro (quasiquote x)
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
       ((and (eq? (car exp) 'unquote) (eqv? (length exp) 2))
	(if (eqv? nesting 0)
	    (second exp)
	    (combine-skeletons ''unquote 
			       (expand-quasiquote (cdr exp) (- nesting 1))
			       exp)))
       ((and (eq? (car exp) 'quasiquote) (eqv? (length exp) 2))
	(combine-skeletons ''quasiquote 
			   (expand-quasiquote (cdr exp) (+ nesting 1))
			   exp))
       ((and (pair? (car exp))
	     (eq? (caar exp) 'unquote-splicing)
	     (eqv? (length (car exp)) 2))
	(if (eqv? nesting 0)
	    (list 'append (second (first exp))
		  (expand-quasiquote (cdr exp) nesting))
	    (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
			       (expand-quasiquote (cdr exp) nesting)
			       exp)))
       (else (combine-skeletons (expand-quasiquote (car exp) nesting)
				(expand-quasiquote (cdr exp) nesting)
				exp))))
    (expand-quasiquote x 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply x y) (.apply x y))

(define map 
  (lambda (F . Lists)
    (define (firsts L) (if (null? L) () (cons (first (first L)) (firsts (rest L)))))
    (define (rests L) (if (null? L) () (cons (rest  (first L)) (rests  (rest L)))))
    (if (null? (first Lists)) ()
        (cons (apply F (firsts Lists))
              (apply map (cons F (rests Lists)))))))

(define for-each
  (lambda (F . Lists)
    (define (firsts L) (if (null? L) () (cons (first (first L)) (firsts (rest L)))))
    (define (rests L) (if (null? L) () (cons (rest  (first L)) (rests  (rest L)))))
    (if (null? (first Lists)) ()
        (begin
           (apply F (firsts Lists)) 
           (apply for-each (cons F (rests Lists)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define-macro (let bindings . body)
    (define (varval v) (jsint.Symbol.intern (.concat (.toString v) "=")))
    (define (named-let name bindings body)
       ((lambda (new-bindings)		; Can't use let yet!
	 `(let ,(cons `(,name #f) new-bindings)
	    (set! ,name (lambda ,(map first bindings) . ,body))
	    (,name . ,(map car  new-bindings))))
	(map (lambda (b) `(,(varval (car b)) ,(cadr b))) bindings)))
    (if (symbol? bindings) 
	(named-let bindings (first body) (rest body))
	`((lambda ,(map first bindings) . ,body) . ,(map second bindings))))

(define-macro (let* bindings . body )
     (if (null? bindings) (jsint.Scheme.toBody body)
	 (if (null? (cdr bindings))
	     `(let (,(first bindings)) . ,body)
	     `(let (,(first bindings))
		(let* ,(rest bindings) . ,body)))))

(define-macro (letrec bindings . body)
    (let ((vars (map first bindings))
	  (vals (map second bindings)))
    `(let ,(map (lambda (var) `(,var #f)) vars)
       ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
       ,(jsint.Scheme.toBody body))))
    
(define-macro (case exp . cases)
    (define (do-case case)
      (cond ((not (pair? case)) (error '(bad syntax in case:) case))
	    ((eq? (first case) 'else) case)
	    (else `((member <exp> ',(first case)) . ,(rest case)))))
    `(let ((<exp> ,exp)) (cond . ,(map do-case cases))))

(define-macro (do bindings test-and-result . body)
    (let ((variables (map first bindings))
	  (inits (map second bindings))
	  (steps (map (lambda (clause)
			(if (null? (cddr clause))
			    (first clause)   
			    (third clause)))
		      bindings))
	  (result (if (null? (cdr test-and-result)) ''unspecified
		      `(begin . ,(cdr test-and-result)))))
      `(letrec ((<loop>
		 (lambda ,variables
		   (if ,(first test-and-result)
		       ,result
		       (begin 
			 ,@body
			 (<loop> . ,steps))))))
	 (<loop> . ,inits))))

(define-macro (delay . exp)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arithmetic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (number? N) (.isInstance Number.class N))
(define = Op.eq)
(define < Op.lt)
(define > Op.gt)
(define <= Op.le)
(define >= Op.ge)

(define + (lambda R (Op.addMulti R)))
(define * (lambda R (Op.mulMulti R)))

(define - (lambda R
  (cond ((null? R) 0)
        ((null? (cdr R)) (Op.sub 0 (car R)))
        ((null? (cddr R)) (Op.sub (car R) (cadr R)))
        (else (throw (list "wrong number of arguments to \"-\" in " (cons "-" R)))))))

(define (/ a b)  (Op.div a b))

(define max 
 (letrec ((iter (lambda (x L) (if (null? L) x (let ((y (first L))) (iter (if (> x y) x y) (rest L)))))))
  (lambda L
   (iter (first L) (rest L)))))

(define min 
 (letrec ((iter (lambda (x L) (if (null? L) x (let ((y (first L))) (iter (if (< x y) x y) (rest L)))))))
  (lambda L
   (iter (first L) (rest L)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; use-module
;;
;; general form is some prefix of
;; (use-module FILENAME SPECIFIER SYMBOLS PREFIX)
;; where
;;    FILENAME is a filename, URL, or resource name
;;    SPECIFIER is one of 'import-procedures, 'import-macros, 'import  --- default is import-procedures
;;    SYMBOLS is either 'all or a list of symbols '(a b c ...) -- default is 'all
;;    PREFIX is a prefix string -- default is ""
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
