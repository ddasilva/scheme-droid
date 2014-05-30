;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prims.scm
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

; <a name="cons"></a>
(define cons Pair.)

; <a name="first"></a>
(define first .first$)

; <a name="rest"></a>
(define rest .rest$)

; <a name="list"></a>
(define list (set-name! 'list (lambda R R)))
(define car .first$)
(define cdr .rest$)
(define (set-car! L v) (.first$ L v))
(define (set-cdr! L v) (.rest$ L v))


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

(define reverse .reverse)

(define list-tail .listTail)

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

;; here we define eq? to hold for all scalar quantities
(define (eq? x y) 
  (or 
     (Op.sameObject x y) 
     (and (or (.isInstance java.lang.Character.class x) 
              (.isInstance java.lang.Boolean.class x) 
              (.isInstance java.lang.Number.class x) 
            )
         (.equals x y))))

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

;; a-lists

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


;;  (// "========== SECTION 6.1 BOOLEANS ==========")
;; not, boolean? defined above

;;  (// "========== SECTION 6.2 EQUIVALENCE PREDICATES ==========")
;; eqv? eq? equal? defined above

;;  (// "========== SECTION 6.3 LISTS AND PAIRS ==========")
;; the following are defined above
;;
;; pair? cons car cdr first rest null? list length append reverse list-tail list-ref
;;  second third fourth fifth sixth seventh eight ninth tenth
;;   caar cadr cdar cddr
;;     caaar  caadr  cadar         cdaar  cdadr  cddar  cdddr
;;    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
;;    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr) 1)
;; set-car! set-cdr!
;; memq memv member assq assv assoc

(define (list? L) (if (null? L) #t (and (pair? L) (list? (cdr L))))) ;; this needs (and ..)



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
(define atan (lambda (x . R) 
                 (if (null? R)
                     (java.lang.Math.atan (.doubleValue x))
                     (java.lang.Math.atan (.doubleValue x) (.doubleValue (first R)))))))
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

(define vector (lambda L (list->array Object.class L)))
(define (vector-length v) (java.lang.reflect.Array.getLength v))
(define (vector-ref v n) (java.lang.reflect.Array.get v n))
(define (vector-set! v n x) (java.lang.reflect.Array.set v n x))
(define (vector->list L) (array->list L))
(define (list->vector L) (list->array Object.class L))
(define vector-fill! jsint.U.vectorFill)

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

(define call-with-current-continuation call/cc)

;; (define (eval L) (Scheme.eval L Environment.GLOBAL$)) ;;;;; USES jsint eval!

;;  (// "========== SECTION 6.10 INPUT AND OUPUT ==========")
(define (call-with-input-file filename proc)
  (jsint.Procedure.tryCatch (lambda()
    (let ((in (open-input-file filename))
          (result (.apply proc (list filename))))
    (begin
       (if (not (eq? in null)) (.close in))
       result)))))

(define (call-with-output-file filename proc)
  (jsint.Procedure.tryCatch (lambda()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (intial-environment) (.getInitialEnvironment (Scheme.currentEvaluator)))
(define (interaction-environment) (.getInteractionEnvironment (Scheme.currentEvaluator)))
(define (null-environment) (.getNullEnvironment  (Scheme.currentEvaluator)))

(define (synchronize obj f) (jsint.Procedure.synchronize obj f))

(define (string->expr x) (car (jscheme.REPL.parseScheme x)))
(define string->exprlist jscheme.REPL.parseScheme)
(define load-environment jsint.Scheme.loadEnvironment)
(define environment-bindings jsint.DynamicEnvironment.getBindings)
(define environment-import Scheme.environmentImport)
(define language-import Scheme.languageImport)
(define values (lambda R (jsint.Values.values R)))
(define call-with-values jsint.Values.callWithValues)
(define null #null)
(define (complex? x) #f)
(define (rational? x) #f)
