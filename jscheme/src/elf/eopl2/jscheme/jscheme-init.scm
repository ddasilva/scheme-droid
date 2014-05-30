{
<html><body>
<h1>EOPL2 support</h1>
EOPL2 (Essentials of Programming Languages Second Edition)
compatibility for Jscheme. http://www.cs.indiana.edu/eopl/

<p>
Say, the eopl2 software (*.scm files) is in the directory src/eopl2 and
the jscheme.jar is in lib/jscheme.jar.  Then you can invoke JScheme
like this (on Windows):

<pre>
java -classpath "src/eopl2;lib/jscheme.jar" jscheme.REPL elf/eopl2/jscheme/jscheme-init.scm
</pre>
To run the tests, evaluate (do-test).
Takes about 13 minutes to run.
based on measurements kanderson@bbn.com made on 28FEB02, 700MHz Pentium

<p>
A pretty-printer is provided from <a href="http://www.swiss.ai.mit.edu/~jaffer/SLIB.html">slib</a> by Mark Feeley.

<p>For define-syntax you can use either Kent Dybvig's psyntax package,
or Dorai Sataram's mbe.scm.  Kent's is more powerful and hygenic but
makes debugging in JScheme much harder.  Dorai's is simpler and is a
little faster, so by default we use it.
<pre>
}
(load "elf/basic.scm")
;; (load "elf/eopl2/jscheme/psyntax-init.scm")
(load "elf/mbe.scm")
(load "r5rs.scm")

;;; Use the Mark Feeley's pretty printer from slib.
(load "elf/eopl2/jscheme/genwrite.scm")
(set! sllgen:pretty-print pretty-print)
(set! eopl:pretty-print pretty-print)
(set! define-datatype:pretty-print pretty-print)

(define eopl:error
  (lambda (who format . data)
    ;; print the message
    (eopl:printf "Error reported by ~s:~%" who)
    (apply eopl:printf (cons format data))
    (newline)
    (error "Yow!")))

(define eopl:error
  (lambda (who format . data)
    (error who format data)))

(load "sllgen.scm")
(load "define-datatype.scm")
(load "test-harness.scm")
(load "test-suite.scm")
;;; (load "6-top.scm")
;;; (set! stop-after-first? #f)

(define eopl:error-stop
  (lambda args (error args)))

;;; Reather than using dynamic-wind and call/cc tryCatch does what we want.
(define safely-run-experiment-on-program
  (lambda (experiment pgm error-val)
    (tryCatch
     (experiment pgm)
     (lambda (e) error-val))))

(define define-datatype:tester
  (lambda (example)
    (display "------------------------------")
    (newline)
    (sllgen:pretty-print example)
    (display "-->")
    (newline)
    (tryCatch
     (begin
       (write (eval example (interaction-environment)))
       (newline)
       #t)
     (lambda (e) #f))))

(define pretty-expand (lambda (e) (pretty-print (sc-expand e))))

(define (do-test)
  ;; Run all the tests, then exit!
  (load "all-tests.scm")
  (display (time (do-all-tests) 1))
  (newline)
  (exit))
{</pre></body></html>}