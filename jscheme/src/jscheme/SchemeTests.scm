{
JScheme test suite.

These tests are in the order they were used for debugging.  So, it is
fairly good history of Jscheme bugs.  The original version we got from
Peter Norvig was 2500 lines of compiled code, but he couldn't run it,
or test it.  So, the first test, typing 3 to the REPL, failed.  It
took about 60 tests to make JScheme usable.  This suggests you should
expect one bug per page of code.

(run-tests debug?) will run the tests.  Printing all tests if debug
is #t.

(define-test) will define a test.
(define-tests) defines tests in bulk:
A test is either
- a string "mm/dd/yyyy" indicating the date the following tests were written.
- an expression, which is (eval)'ed
- `(,eq ,expression ,expect) which is an expression and its expected 
  value.  Where eq can be one of '(= eq? eqv? equal?)
- `(true ,expression) which is an expression that must be true.
- `(,let ,args .tests) A one level let, let* or letrec that can have
test expressions and other expressions in its body, see examples below.
Side effects in let body are evaluated at load time, not at
run-tests time.

(test-history) will print data you can feed to EXCEL to plot the test history.
}
(use-module "elf/basic.scm" 'import 'all)
(use-module "elf/mbe.scm" 'import 'all)
(use-module "elf/serialize.scm" 'import 'all)

(import "java.util.Hashtable")
(import "javax.swing.JFrame")
(import "jscheme.JScheme")

(define (test-date? t) (and (pair? t) (eq? (car t) 'date)))

(define (serialize-test)
  ;; A better test might be to restore the heap in a separate JVM.
  ;; But this should catch most problems.
  (tryCatch
   (begin (saveHeap "heap.ser")
	  (restoreHeap "heap.ser")
	  (.delete (java.io.File. "heap.ser"))
	  #t)
   (lambda (e) (display e) (newline) '#f)))
   
;; used in a test
(define-method (foo (x Integer)) (list 'int x))
(define-method (bar (x Symbol)) x)
(define-method (bar (x Number)) (* 2 x))
(define-method (bar (x Byte)) (* 8 x))

;;; Used in a test
(define-syntax casequal
  (syntax-rules (else)
    ((casequal val ((m1 m2 ...) e1 e2 ...) clauses ...)
     (let ((key val))
       (cond ((member key '(m1 m2 ...)) e1 e2 ...)
             (else (casequal key clauses ...)))))
    ((casequal val (else e1 e2 ...)) (begin e1 e2 ...))
    ((casequal val) (if #f #f))))

(set! debug #t)				; This turns on U.p().

(define-macro (define-tests . forms)
  `(begin ,@(map (lambda (f) `(define-test ,f)) forms)))

(begin					; Don't print this!
  (set! test-queue			; Norvig queue of tests.
	(let ((it (cons '() '())))
	  (set-car! it it)))
  #f)

(define (enqueue-test test)
  (set-cdr! (car test-queue) (cons test '()))
  (set-car! test-queue (cdar test-queue)))

(define (tests) (cdr test-queue))

(define-macro (define-test form)
  (cond ((test? form) (define-test-test form))
	((let? form) (define-test-let form))
	((string? form) (define-test-date form))
	(else (error "Don't know how to compile " form))))

(define (test? exp)
    (and (pair? exp) (memq (car exp) '(= eq? eqv? equal? true))))

(define (let? exp) (and (pair? exp) (member (car exp) '(let let* letrec))))

(define (define-test-test form)
  `(make-test ,`(lambda () ',form) (lambda () ,form)))

(define (equality-test? test)
  (and (pair? test) (member (car test) '(= eqv? equal? eq?))))

(define (make-test text test)
  (enqueue-test (cons text test)))
(define test-text car)
(define test-test cdr)
(define (run-test test debug)
  (let ((result ((test-test test))))
	(if (or debug (not result))
	    (begin
	      (write ((test-text test)))
	      (display {  [(if result ""
			       "failed!")] \n})))
	result))

(define (true x) x)

(define (define-test-let form)
  ;; Allow one level of let.
  (define let-kind car)
  (define let-parameters cadr)
  (define let-body cddr)
  (define (define-test-let-form form)
    (if (test? form) `(define-test ,form)
	form))
  `(,(let-kind form)
    ,(let-parameters form)
    ,@(map define-test-let-form  (let-body form))))

(define (define-test-date form) `(make-test 'date ,form))

(define (scheme-number string)
  (let ((old U.useJavaSyntax$))
    (set! U.useJavaSyntax$ #f)
    (let ((it (car (jscheme.REPL.parseScheme string))))
      (set! U.useJavaSyntax$ old)
      it)))

(define-tests
  "12/02/1999"  ;; KRA Nothing works
  (equal? (= 3 0) #f)
  (= 3 3)
  (= (sin 0.0) 0.0)
  (eq? 'x (quote x))
  (equal? (list 3) '(3))
  (equal? (list 3 4) '(3 4))
  (equal? (cons 3 4) '(3 . 4))
  (equal? (cons 3 (list 4)) '(3 4))
  (equal? #(1) (vector 1))
  (equal? #(2 3) (vector 2 3))
  (= (+) 0)
  (equal? '(1 2) (list 1 2))
  (= 2 2)
  (= (/ 3 4) 0)
  (= (+ 1 2 3) 6)
  (true (procedure? (lambda () 3)))
  (= ((lambda () 3)) 3)
  (let ((x 3))
    (= ((lambda (x) 3) 15) 3)
    (= ((lambda (x) x) 15) 15)
    (= ((lambda (x) (if (> x 3) 5 9)) 2) 9)
    (= ((lambda (x) (if (> x 3) 5 9)) 17) 5)
    (= ((lambda (x y) (+ x y)) 5 2) 7)
    )
  (equal? ((lambda z z) 1 2 3 4) '(1 2 3 4))
  (equal? (apply list '(1 2 3 4)) '(1 2 3 4))
  (= (apply + '(1 2 3)) 6)
  (= (apply + '(1 2 3)) (+ 1 2 3))

  (equal? '(a . b) (cons 'a 'b))
  (equal? ((lambda (x . b) b) 2 3) '(3))
  ;;    (= (/ 3) (/ 1 3))
  (equal? ((lambda z z) 1 2 3) '(1 2 3))
  (equal? (apply (lambda z z) '(1 2 3)) '(1 2 3))

  (= (eval ((macro (x) (list '+ x 3)) 3)) 6)

  (eqv? (let ()
	  (define (f x) (+ x 3))
	  (f 5))
	8)

  (equal? (reverse '(1 2 3)) '(3 2 1))
  (= ((lambda (x) (define (f x) (* x x)) (f (f x))) 3) 81)

  (equal? (let ()
	    (define (g x)
	      (define (sq y) (* (f y) y))
	      (define (f a) (+ a x))
	      (sq (sq x)))
	    (g 3))
	  378)

  (equal? (let ()
	    (define (g x) 
	      (letrec ((sq (lambda (y) (* (f y) y)))
		       (f  (lambda (a) (+ a x))))
		(sq (sq x))))
	    (g 3))
	  378)

  (equal? (let ((x 3) (y 4)) (* x y)) 12)
  (equal? (let* ((x 3) (y (* x 2))) (+ x y)) 9)

  (equal? (map (lambda (var val) `(set! ,var ,val)) '(a b c) '(1 2 3))
	  '((set! a 1) (set! b 2) (set! c 3)))

  (equal? (append) '())
  (equal? (append '()) '())
  (equal? (append () ()) '())
  (equal? (append '(1 2) ()) '(1 2))
  (equal? (append '(1 2) '(3 4)) '(1 2 3 4))
  (equal? (append 3) 3)
  ;; ??? (append 3 3)
  (let ((x 3)
	(y '(1 2)))
    (equal? `(,x ,y) '(3 (1 2)))
    (equal? `(,x ,@y) '(3 1 2)))

  (equal? (case 3 ((1 2) 'low) ((3) 'hi) ((4 5) 'very high)) 'hi)

  (equal? (.first$ (cons 1 2)) 1)
  (equal? (let ((it (cons 1 2)))
	    (.first$ it 3)
	    it)
	  '(3 . 2))

  (equal? (list->vector '()) #())
  (equal? (list->vector '(1 2 3)) (vector 1 2 3))
  (equal? (class 'Hashtable) (class 'java.util.Hashtable))
  (equal? (class 'JFrame) (class 'javax.swing.JFrame))
  (let ((h (new 'java.util.Hashtable)))
    (invoke h 'put 'ken 3)
    (invoke h 'put 'tim 3)
    (= (invoke h 'get 'ken) 3)
    (= (invoke h 'get 'ken) (invoke h 'get 'tim))
    (equal? (.get h 'ken) 3)
    (equal? (.get h 'tim) ((method "get" "Hashtable" "Object") h 'tim)))

  (true (not (null? (invoke-static 'System 'getProperty "user.dir"))))

  (let ((x (new 'jsint.Pair 1 2)))
    (equal? x '(1 . 2))
    (equal? (peek x "first") 1)
    (equal? (peek x "rest") 2)
    (equal? (invoke-static 'jsint.Symbol 'intern "x") 'x)
    ;; (equal? (.getGlobalValue (invoke-static 'jsint.Symbol 'intern "x")) x)
    )
  (let ((x (new 'jsint.Pair 1 2)))
    (poke x "rest" 4)
    (poke x "first" 7)
    (equal? x '(7 . 4)))
  
  (equal? (peek-static 'jsint.Symbol "SET") 'set!)                 
  (equal? (Hashtable. 10) ((constructor "java.util.Hashtable" "int") 10))


  "12/28/1999" ; KRA
  (equal? (list->string (string->list "abc")) "abc")
  (equal? (list->string (string->list "")) "")
  (let ()
    (define (g x)
      (lambda (y) (+ x y)))
    (equal? (equal? (g 3) (g 3)) #f))
  (equal? (substring "012345" 4 6) "45")

  "12/29/1999"				; KRA
  ;;    (equal? (/ 3) (/ 1 3))
  (equal? (- 3) -3)

  "12/30/1999" ; KRA
  (equal? (let ((v (vector 1 2 3))) (vector-set! v 2 4) v) #(1 2 4))

  "1/03/2000" ;; KRA Did not compile.
  (let ()
    (define f (lambda () ()))
    (equal? (f) '()))

  "1/31/2000" ;; KRA
  (equal? (equal? (list #null) (list #null)) #t)
  (equal? (expt 10 2) 100.0)

  "5/14/2000" ;; KRA 14MAY00: correct behavior of U.toStr() and (string-append).
  (equal? (string-append #\a #\b #\c) "abc")
  (equal? (string-append "hi" 'buster 3) "hibuster3")
  (equal? (string->list "abc") '(#\a #\b #\c))

  "7/26/2000" ;; KRA 26JUL00: Test jsint.SI.class
  (equal? (.eval (JScheme.forCurrentEvaluator) '(+ 2 3)) (+ 2 3))
  (equal? (.eval (JScheme.forCurrentEvaluator) "(+ 2 3)") (+ 2 3))
  (equal? (.call (JScheme.forCurrentEvaluator) "+" 2 3) (+ 2 3))
  (equal? (.call (JScheme.forCurrentEvaluator) + 2 3) (+ 2 3))
  (equal? (.apply (JScheme.forCurrentEvaluator) "+" (JScheme.list 2 3)) (+ 2 3))
  (let ()
    (.load (JScheme.forCurrentEvaluator) "(define (si:f x) (+ x (si:g x))) (define (si:g x) (* x 3))")
    (equal? (si:f 3) 12))

  "8/7/2000" ;; KRA 07AUG00: Bug in Pair.equal.
  (equal? '(#(1). #(2)) '(#(1) . #(2)))
  (equal? '(1 2 "hi") '(1 2 "hi"))
  (equal? (.hashCode '(#(1). #(2))) (.hashCode '(#(1). #(2))))

  "10/29/2000";; KRA 29OCT00: Boolean eq? failed.
  (equal? (eq? (Boolean. #t) #t) #t)
  (equal? (eq? (Boolean. #f) #f) #t)
  (equal? (eq? #t (Boolean. #t)) #t)
  (equal? (eq? #f (Boolean. #f)) #t)
  (equal? (eq? #t #t) #t)
  (equal? (eq? #f #f) #t)

  "11/28/2000" ;; KRA 28NOV00: Truncate and several other procedures failed.
  (eqv? (max 3 4) 4)
  (eqv? (max 3.9 4) 4.0)

  (eqv? (+ 3 4) 7)
  (eqv? (+ 3) 3)
  (eqv? (+) 0)
  (eqv? (* 4) 4)
  (eqv? (*) 1)

  (eqv? (- 3 4) -1)
  ;; (eqv? (- 3 4 5) -6)		; KRA 28NOV00: - takes 1 or 2 args.
  (eqv? (- 3) -3)
  ;; (eqv? (/ 3 4 5) 0)		; KRA 28NOV00: / takes 2 args.
  ;; (/ 3)				
	  
  (eqv? (abs -7) 7)
  (eqv? (remainder  13  4)  1)
  (eqv? (modulo    -13  4)  3)
  (eqv? (remainder -13  4) -1)
  (eqv? (modulo     13 -4) -3)
  (eqv? (remainder  13 -4)  1)

  (eqv? (modulo     0  5) 0)
  (eqv? (modulo     -5  5) 0)
  (eqv? (modulo      5 -5) 0)
  (eqv? (remainder     0  5) 0)
  (eqv? (remainder    -5  5) 0)
  (eqv? (remainder     5 -5) 0)


  ;;    (eqv? (remainder -13 -4.0) -1.0) ;; TJH 8/31/2001  THIS TEST FAILS AS IT RETURNS -1 and not -1.0
  ;; BUG

  (eqv? (gcd 32 -36) 4)
  ;; (eqv? (gcd) 0)			; KRA 28NOV00: takes 1+ args.
  (eqv? (lcm 32 -36) 288)
  (= (lcm 32.0 -36) 288.0)  ;; TJH 31AUG01 standard only requires "=" equality for non-integer args
  (eqv? (lcm) 1)

  (= (floor -4.3) -5.0)
  (= (ceiling -4.3) -4.0)
  (= (truncate -4.3) -4.0)
  (= (round -4.3) -4.0)
    
  (= (floor 3.5) 3.0)
  (= (ceiling 3.5) 4.0)
  (= (truncate 3.5) 3.0)
  (= (round 3.5) 4.0)
  (eqv? (round 7) 7)

  "1/9/2001"
  ;; KRA 09JAN01: (integer->char 128) caused out of bounds exception.
  (eq? (integer->char 0) (integer->char 0)) ; Cached.
  (eq? (integer->char 127) (integer->char 127))
  ;;; These tests check if we can construct such characters:
  (eqv? (char->integer (integer->char -3)) 65533)
  (eqv? (char->integer (integer->char 128)) 128)
  (eqv? (char->integer (integer->char 12345)) 12345)

  "6/27/2001"
  ;; KRA 27JUN01: #\s #\S #\n and #\N were not read properly.
  (eqv? #\s #'s')
  (eqv? #\S #'S')
  (eqv? #\n #'n')
  (eqv? #\N #'N')
  (eqv? #\space #' ')
  (eqv? #\newline #'\n')
  (eqv? #\SPACE #' ')
  (eqv? #\NEWLINE #'\n')
  (eqv? #'&' #'\u0026')
  (eqv? #'\046' #'&')
  (eqv? #\' #'\'')
  (eqv? #\" #'\"')
  (eqv? #\\ #'\\')
  (eqv? #\# #'#')

  "8/31/2001"
  ;;  TJH 31AUG01: New Exception handling.
  (true (.isInstance java.lang.ArithmeticException.class (tryCatch (/ 1 0) (lambda(e) e))))
  (true (.isInstance jscheme.SchemeException.class (tryCatch (a b) (lambda(e) e))))
  (true (.isInstance java.lang.ArithmeticException.class 
		     (tryCatch 
		      (tryCatch (/ 1 0) (lambda(e) (throw e)))
		      (lambda(e) e))))

  "11/23/2001"
  ;; KRA 23NOV01: (list? 3) errored!
 (eq? (list? '(a b c)) #t)
 (eq? (list? '()) #t)  
 (eq? (list? '(a . b)) #f)

;;; BUG this cyclic structure kills the jscheme/BootstrapCore.scm version...
; (let ((x (list 'a)))
;   (set-cdr! x x)
;   (eq? (list? x) #f))

 (eq? (list? 3) #f)  

  "12/8/2001" ;; TJH 8DEC01
  (equal? (make-vector 3 2) #(2 2 2))
  (equal? (make-vector 3) #(#null #null #null))

  "3/9/2002"
  ;; TJH 9Mar02, tests of quasi-string notation
  ;; and exception of symbols containing array marker []
  (equal? {abc"[(+ 1 2)]"\{\}\[\]} "abc\"3\"{}[]")
  (equal? {a[(if (< 1 2) {bc} {de})]d} "abcd")
  (equal? 'Object[] (string->symbol "Object[]"))
  (equal? {static ['Object[]] x;} "static Object[] x;")
  (equal? Object[].class (Class.forName "[Ljava.lang.Object;"))
  (equal? {[String[].class]} "class [Ljava.lang.String;")

  "3/10/2002"
  ;; TJH 3/10/02 -- tests in response to Bill Hale's bug report
  (eqv? (modulo  123456789  100)  89)
  (eqv? (modulo  123456789 -100) -11)
  (eqv? (modulo -123456789  100)  11)
  (eqv? (modulo -123456789 -100) -89)

  ;;  (eqv? (modulo 123456789012 100) 12)  ;; BUG this currently Fails!!

  
;; TJH 3/10/02 -- tests in response to Bill Hale's list-tail bug report
  (equal? (list-tail '(1 2 3) 3) '())

  (equal? (tryCatch (list-tail '(1 2 3) 4) (lambda(e) 'error)) 'error)

  (equal? (list-tail '(1 2 3) 1) '(2 3))

  "3/11/2002"
  ;; KRA 11MAR02: Derek Upham <Derek.Upham@ontain.com> found a bug in cond
  ;; i introduced:
  (eq? (cond ((eq? 'a 'a) => (lambda (x) x))) #t)


  ;; TJH 3/11/02 -- check that Bytes and Shorts are handled properly by eqv? and equal?
  ;; KRA 22JUL04: These tests now fail with current definition of eqv.

  (eqv? (eqv? 10 (java.lang.Byte. "10")) #f)
  (eqv? (eqv? 10L (java.lang.Byte. "10")) #f)
  (eqv? (eqv? 10 (java.lang.Short. "10")) #f)
  (eqv? (eqv? (java.lang.Byte. "10") (java.lang.Short. "10")) #f)

  (eqv? (equal? 10 (java.lang.Byte. "10")) #f)
  (eqv? (equal? 10 (java.lang.Short. "10")) #f)
  (eqv? (equal? 10 (java.lang.Short. "10")) #f)
  (eqv? (equal? (java.lang.Byte. "10") (java.lang.Short. "10")) #f)

  "3/14/2002"
  ;; KRA 31DEC03: list->String[] was only used in build/bootstrap.scm
  ;; which is no longer used.
  ;; (equal? (list->String[] '(1 2 3)) (array String.class "1" "2" "3"))

  ;; Derek Upham 12MAR02: 
  ;; Dorai Sitaram's "Macros By Example" implementation uploaded
  ;; yesterday had a slight violation of R5RS semantics (it was
  ;; treating pattern keywords as literal identifiers, which messed up
  ;; recursive macro definitions).

  (equal? (casequal "foo" (("foo") 1) (("bar") 2)) 1)
  (equal? (casequal "bar" (("foo") 1) (("bar") 2)) 2)
  (equal? (casequal "baz" (("foo") 1) (("bar") 2)) #f)
  (equal? (casequal "baz" (("foo") 1) (("bar") 2) (else 3)) 3)

  "3/18/2002"
  ;; Derek Uphap 18MAR02
  ;; (define-method (foo (x Integer)) (list 'int x))
  ;; (define-method (bar (x Symbol)) x)
  ;; (define-method (bar (x Number)) (* 2 x))
  ;; (define-method (bar (x Byte)) (* 8 x))

  (equal? (foo 3) '(int 3))
  (equal? (tryCatch (foo 3L) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (foo (Byte. "3")) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (foo 'apple) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (bar "apple") (lambda(e) 'error)) 'error)
  (equal? (bar (Byte. "3")) 24)
  (equal? (bar 3) 6)
  (equal? (bar 3L) 6L)
  (equal? (bar 'L) 'L)

  ;; enhanced javadot tests
  ;; private fields
  (equal? (tryCatch (.name$  'a) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.name$# 'a) (lambda(e) 'error)) "a")

  (equal? (tryCatch (.Symbol.name$  'a) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.Symbol.name$# 'a) (lambda(e) 'error)) "a")

  (equal? (tryCatch (.jsint.Symbol.name$  'a) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.jsint.Symbol.name$# 'a) (lambda(e) 'error)) "a")

  ;; private instance methods
  (equal? (tryCatch (.equalsFirst '(a b) '(a c)) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.equalsFirst# '(a b) '(a c)) (lambda(e) 'error)) #t)

  (equal? (tryCatch (.Pair.equalsFirst '(a b) '(a c)) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.Pair.equalsFirst# '(a b) '(a c)) (lambda(e) 'error)) #t)

  (equal? (tryCatch (.jsint.Pair.equalsFirst '(a b) '(a c)) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.jsint.Pair.equalsFirst# '(a b) '(a c)) (lambda(e) 'error)) #t)

  ;; private static methods
  (equal? (tryCatch (Pair.hashCode0  #null) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (Pair.hashCode0#  #null) (lambda(e) 'error)) 17)

  (equal? (tryCatch (jsint.Pair.hashCode0  #null) (lambda(e) 'error)) 'error)
  (equal? (tryCatch (jsint.Pair.hashCode0#  #null) (lambda(e) 'error)) 17)

  ;; private constructors
  (equal? (tryCatch (Symbol. "abc") (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.name$# (Symbol.#  "abc")) (lambda(e) 'error)) "abc")

  (equal? (tryCatch (jsint.Symbol. "abc") (lambda(e) 'error)) 'error)
  (equal? (tryCatch (.jsint.Symbol.name$# (jsint.Symbol.#  "abc")) (lambda(e) 'error)) "abc")

  "4/13/2002"
  ;; TJH 13April02,  new byte/short/long/float input/output formats
  (equal? (.getClass 0b) java.lang.Byte.class)
  (equal? (.getClass 0B) java.lang.Byte.class)
  (equal? (.getClass 0s) java.lang.Short.class)
  (equal? (.getClass 0S) java.lang.Short.class)
  (equal? (.getClass 0l) java.lang.Long.class)
  (equal? (.getClass 0L) java.lang.Long.class)
  (equal? (.getClass 0f) java.lang.Float.class)
  (equal? (.getClass 0F) java.lang.Float.class)
  (equal? (jsint.U.stringify 0b) "0B")
  (equal? (jsint.U.stringify 0s) "0S")
  (equal? (jsint.U.stringify 0 ) "0")
  (equal? (jsint.U.stringify 0L) "0L")
  (equal? (jsint.U.stringify 0f) "0.0F")
  (equal? (jsint.U.stringify 0.0) "0.0")

  ;; TJH 13April02, test removal of widening/narrowing code
  (equal? (tryCatch 
            (let ((x (java.lang.reflect.Array.newInstance byte.class 10)))
              (Array.setByte x 5 10)
              (Array.getByte x 5))
            (lambda(e) 'failed))
         'failed)

  (equal? (tryCatch 
            (let ((x (java.lang.reflect.Array.newInstance byte.class 10)))
              (Array.setByte x 5 10b)
              (Array.getByte x 5))
            (lambda(e) 'failed))
         10B)

  "4/19/2002"
  ;; KRA 19APR02: Bug found by Clint Hyde.
  (equal? (tryCatch (.Vector) (lambda (e) 'error)) 'error)
  (equal? (tryCatch (.Vector 3) (lambda (e) 'error)) 'error)

  "8/22/2002"
  ;; KRA 22AUG02: Test new version of !{}.
  (equal? {["a"]} "a")
  (equal? {[{a}]} "a")
  "11/16/2002"
  ;; This threw an exception.
  (eq? (equal? 1.0 "Free" ) #f)
  "12/07/2002"
  (equal? (values 3) 3)
  (equal? ((lambda () (values 1 2) 3)) 3)
  (equal? (call-with-values (lambda () 3) (lambda (x) x)) 3)
  (equal? (call-with-values (lambda () (values)) (lambda () 3)) 3)
  (equal? (call-with-values (lambda () (values 3)) (lambda (x) x)) 3)
  (equal? (call-with-values (lambda () (values 1 2)) (lambda (a b)
						       (vector b a))) '#(2 1))
  (equal? (call-with-values (lambda () (values))
			    (lambda args
			      (list->vector args))) '#())
  (equal? (call-with-values (lambda () (values 1))
			    (lambda args
			      (list->vector args))) '#(1))
  (equal? (call-with-values (lambda () (values 1 2))
			    (lambda args
			      (list->vector args))) '#(1 2))
  (equal? (.toString (values 1 2))
	  (string-append "1" (System.getProperty "line.separator") "2"))

  "01/06/2003" ;; Tests for Queue objects.
  
  (equal? (.getContent (.add (.add (jsint.Queue. 3) 4) 5)) '(3 4 5))
  (eqv? (.pop (.add (.add (jsint.Queue. 3) 4) 5)) 3)
  (equal? (.getContent (let ((q (.add (.add (jsint.Queue. 3) 4) 5)))
			 (.pop q)
			 q)) '(4 5))
  (equal? (.getContent (let ((q (.add (.add (jsint.Queue. 3) 4) 5)))
			 (.pop q)
			 (.remove q))) '(5))
  (equal? (let ((q (.add (.add (jsint.Queue. 3) 4) 5)))
	    (.pop q)
	    (.remove q)
	    (.pop q))
	  5)
  (eqv? (let ((q (.add (.add (jsint.Queue. 3) 4) 5)))
	  (.pop q)
	  (.pop q)
	  (.pop q)
	  (.pop q)
	  (.pop q))
	#null)
  "04/07/2003" ;; #xff failed
  (= (scheme-number "#xff") 255)
  (= (scheme-number "#o10") 8)
  (= (scheme-number "#b101") 5)
  "07/21/2003" ;; tail recursive flatten had a bug.
  (equal? (flatten '(() 1 (2) (3) () 4)) '(1 2 3 4))
  (= (length (flatten (let * ((sofar '())
			      (n 10000))
			(if (= n 0) sofar
			    (* (cons '(1 2 3 4 5 6 7 8 9 10) sofar)
			       (- n 1))))))
     100000)
  "01/08/2003" ;; found by r4rstest.scm
  (= (let ((f -)) (let f ((n (f 1))) n)) -1)
  (= (let ((x 34)) (let* () (define x 8) x) x) 34)
  (= (letrec ((x 3)) (define x 10) x) 10)
  "05/05/2004" ;; multiple independent environments
  ;; I use true here to mean run during the test for side effects
  (let ((x 0)
        (js1 (JScheme.))
        (js2 (JScheme.)))
    (true (begin
           (set! x 0)
           (.load js1 "(define x 1)")
           (.load js2 "(define x 2)")))
    (= x 0)
    (= (.eval js1 'x) 1)
    (= (.eval js2 'x) 2)
    (true (set! x 10))
    (= x 10)
    (= (.eval js1 'x) 1)
    (= (.eval js2 'x) 2)
    (true (.eval js1 '(set! x 11)))
    (= x 10)
    (= (.eval js1 'x) 11)
    (= (.eval js2 'x) 2)
    (true (.eval js2 '(set! x 12)))
    (= x 10)
    (= (.eval js1 'x) 11)
    (= (.eval js2 'x) 12)
    (= (+ (.eval js1 '(+ (let ((js3 (JScheme.)))
                           (.load js3 "(define x 3)")
                           (.eval js3 'x))
                         x))
          x)
       24))
  "05/21/04" ; (apply car '(1 2) ()) threw an exception.
  (equal? (apply car '(1 2) '()) 1)

  "06/06/2004" ;; literal syntax, TJH
  ;; Default literal syntax is for Java literals, but string->number
  ;; will treat leading zeroes as decimal if base 10 is specified
  ;; KRA 19NOV04: We no longer do this, it confused too many people.

  (equal? 081 81)
  (equal? (.getClass '0123Dig) jsint.Symbol.class)
  (equal? (.getClass '0123) java.lang.Integer.class)
  (equal? (string->number "081" 10) 81)
  (equal? (string->number "010") 10)
  (equal? (string->number "010" 8) 8)
  (equal? (string->number "081" 8) #f)
  (equal? (string->number "08") 8)
  (equal? (string->number "08" 10) 8)
  (equal? (string->number "10") 10)
  (equal? (string->number "10" 10) 10)

  ;; (equal? (string->expr "010") 8)
  (equal? (string->expr "010") 10)
  ;; (equal? (read (jsint.InputPort. (java.io.StringReader. "010"))) 8)
  (equal? (read (jsint.InputPort. (java.io.StringReader. "010"))) 10)

  "07/22/2004" ; serialization test. KRA
  (equal? (serialize-test) #t)

  "07/22/2004" ; Bugs in eqv? found from discussion with Alan Don0van.
  (eqv? (eqv? 3.0 3) #f)
  (eqv? (eqv? 3.0 3L) #f)
  (eqv? (eqv? 3 3L) #f)
  (eqv? 3 3)
  (eqv? 3L 3L)
  (eqv? 3.0 3.0)
  (eqv? (eqv? (list 1 2) (list 1 2)) #f)

  "07/26/2004" ; Tests for describe and apropos.
  (equal? (describe-object 3)
	  "3
 is an instance of java.lang.Integer

  // from java.lang.Number

  // from java.lang.Integer
  value: 3
"
	  )
  (eq? (.startsWith (describe-object let) "Macro named let") #t)
  (eq? (.startsWith (describe-object describe-object)
		   "{jsint.Generic describe-object[0,n]} is a generic") #t)
  (eq? (.startsWith (describe-object int.class)
		    "class: int\nprimitive public abstract final class int")
       #t)

  (equal? (let ((s (java.io.StringWriter.)))
	    (apropos "apropos" (PrintWriter. s))
	    (.toString s))
	  "apropos: (lambda apropos (pattern . out)...)\n")
  "11/22/04" ; Tests for #null as false.
  (eq? (or #null 3) 3)
  (eq? (not #null) #t)
  (eq? (and #null) #f)
  (eq? (if #null 'bad 'good) 'good)
  (eq? (U.to_bool #null) #f)
  (eq? (not #t) #f)
  ;; Bug found by Alan Donovan.
  (eq? (.getClass (tryCatch (.noMethod 0) (lambda (e) e)))
       jscheme.SchemeException.class)
  (eq? (.getClass (tryCatch (.noMethod# 0) (lambda (e) e)))
       jscheme.SchemeException.class)
  "11/23/04" ; New primitives.
  (eq? (isNull #null) #t)
  (eq? (isNull 3) #f)
  (eq? (!isNull #null) #f)
  (eq? (!isNull #f) #t)
  
;  "06/26/07" ; U.tail
;  (equal? '() (tail '()))
;  (equal? '(1) (tail '(1)))
;  (equal? '(3) (tail '(1 2 3)))
  )					;; end of define-tests.

(define (test-history)
  ;; Suitable for EXCEL
  (display {Date\t#Tests\n})
  (for-each
   (lambda (x) (display {[(car x)]\t[(cdr x)]\n}))
   (let loop ((total 0)
	      (previous (car (tests)))	; A date.
	      (tests (cdr (tests))))
     (if (null? tests) (list (list (cdr previous) total))
	 (let ((head (car tests))
	       (tests (cdr tests)))
	   (if (test-date? head)
	       (cons (list (cdr previous) total) (loop total head tests))
	       (loop (+ total 1) previous tests)))))))


(define (run-tests debug)
  (display "\n\n**************** Running jscheme/ScemeTests.scm ******************\n")
  (display "several warnings should appear below as certain error conditions are tested\n")
  (display "this is to be expected and does not signal a problem with the tests\n\n")
  (let ((total 0)
	(failures 0))
    (for-each
     (lambda (t)
       (if (not (test-date? t))
	   (let ((result (run-test t debug)))
	     (set! total (+ total 1))
	     (if (not result) (set! failures (+ failures 1))))))
     (tests))
    (display (string-append "Tests: " total " Failures: " failures "\n"))
    (display "\nAll tests have completed with any errors as shown above\nTry (run-tests #t) to see both failing and successful tests and their results\n")
    (display "********************* jscheme/SchemeTests.scm has completed ***********************\n\n")
  ))

(run-tests #f)

;;; http://java.sun.com/docs/books/jls/second_edition/html/lexical.doc.html#230798
;;Examples of int literals: 

(define intExamples
  (list 0	2	0372
	;; 0xDadaCafe
	1996
	;; 0x00FF00FF
	))

(define longExamples
  (list 0l	0777L
	;; 0x100000000L
	2147483648L
	;; 0xC0B0L
	))

