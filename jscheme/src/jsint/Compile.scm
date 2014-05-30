;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Compiler.scm
;;    this is a front end to the jsint.compiler.Compiler program
;;    It processes the command line arguments and then makes the
;;    appropriate calls to jsint.compiler.Compiler
;;
;; /**
;;  * @author Timothy J. Hickey, Copyright 2000, tim@cs.brandeis.edu, <a href="license.txt">license</a>
;;  * subsequently modified by Jscheme project members
;;  * Copyright (c) 2000 Ken R. Anderson, Tim Hickey, Peter Norvig
;;  * licensed under zlib licence (see license.txt)
;;  *<a href="license.txt">license</a>
;;  */
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(import "java.lang.*")
;(import "jsint.*")
;(import "java.lang.reflect.*")

(tryCatch 
  (begin
    (jsint.compiler.Reflect.load) 
    (jsint.compiler.CompileReflection.load) 
    (jsint.compiler.Compiler.load))
  (lambda(e) ;; Compiler must be loaded by hand
    #t))

(define (printUsage)
   (.println System.out$ "usage: jsint.Compiler  [--help] [-h] [-verbose] [-v] [--package P] [-p P] file1.scm file2.scm ..."))

(define (processCommandLineArgs L)
  (if (null? L) ()
      (case (first L)
        (("-p" "--package")
            (Array.set Globals PackageName (second L))
            (processCommandLineArgs (rest(rest L))))
        (("-v" "--verbose")
            (Array.set Globals Verbose #t)
            (processCommandLineArgs (rest L)))
        (("-r" "--reflect")
            (Array.set Globals UseReflection #t)
            (processCommandLineArgs (rest L)))
        (("-h" "--help") 
            (printUsage)
            (processCommandLineArgs (rest L)))
        (else
           (let ((x (first L)))
             (cond ((and (.startsWith x "-") (<= (.length x) 2)) 
                       (display (list "unknown flag" x (rest L))) (newline)
                       (printUsage))
                   ((.startsWith x "-")  
                       (processCommandLineArgs 
                           (cons (.substring x 0 2)
                           (cons (string-append "-" (.substring x 2 (.length x)))
                              (rest L)))))
                   (else
                     (cons x
                       (processCommandLineArgs (rest L))))))))))

(define (main shellArgs)
    (tryCatch
      (begin
         (for-each compile-file (processCommandLineArgs (array->list shellArgs)))
         (System.exit 0))
      (lambda (e)
          (display (list "Error while compiling " e)) (newline)
          (if (.isInstance JschemeThrowable.class e)
              (begin (display (.contents$ e)) (newline))
              (tryCatch (.printStackTrace e) (lambda(e) "ERROR"))))))

