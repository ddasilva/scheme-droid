{
<html>
 <head>
  <title>Command line processing</title>
  <link rel="stylesheet" href="style.css">
 </head>
 <body>
 <h1>Command line processing</h1>

<p> This is a simple command line processor.  You can define commands
with the the macro <tt>define-command</tt>.  It has a syntax like <tt>(define)</tt> but with an additional documentation line.
<pre>
(define-command (name . args) documentation . body)
</pre>

It defines a top level procedure called <tt>name</tt> and 
adds the documentation to a -help command.

<p> For example the sh shell script,
<a href="build">build</a> or cmd shell script <a href="build.bat">build.bat</a> uses commands
defined in <a href="build.html">build.scm</a>.
<p>So invoking:
<pre>
bash src/using/build.bash -help
</pre>
<p>Produces
<pre>
make command*

With no commands an interactive prompt is provided.
command ::= -key arg*

Where command is one of:
-help
  Print help message

-clean
  Remove all generated files.

-etags
  Make a TAGS table for EMACS meta-dot.
The command etags must be in your path.

-javac
  Recompile java files that need it.

-javadoc
  Generate API documentation.
Must have run -javac first.

-jar
  Build jar from .class and .scm files
</pre>

<hr>
<h2>Command line processing</h2>

<p>The command line can contain several commands.  The command line is
treated as follows.  Each argument is inspected from left to right.
If the current argument names a command, its arguments are collected,
the command is applied to its arguments (which are Strings) and
command line processing continues.  If the argument starts with "(" it
is evaluated as a JScheme expression, otherwise the argument is
assumed to be a file or resource name and (load) is invoked on it.

<hr>
<h2>Command record type</h2>
<pre>

command ::= `(,name&args ,documentation , procedure)
name&args ::= `(,name ,@args) - name and any arguments of command as in (define).
documentation ::= Documentation String, can be multiple lines.
procedure ::= a procedure applied to its arguments when command is invoked.
}
(load "elf/basic.scm")

(define commandAppName "make")		; Default app name for -help.
(define commands '())		; An alist in revere order of when defined.
(define (addCommand name args documentation procedure)
  (define (grindargs name&args)
    (if (null? name&args) '()
	(if (pair? name&args)
	    (cons (car name&args) (grindargs (cdr name&args)))
	    (list "." name&args))))
  (let ((documentation
	 {[(apply string-append
		  (separate " " (grindargs (cons name args))))]\n  [documentation]\n})
	(cmd (findCommand name)))
    (if cmd (set-cdr! cmd `(,documentation ,procedure))
	(set! commands (cons `(,name ,documentation ,procedure) commands)))))
(define-method (findCommand (name Symbol)) (assoc name commands))
(define-method (findCommand (name String))
  (findCommand (string->symbol name)))

(define commandKeyword? findCommand)
(define commandName car)
(define commandDocumentation cadr)
(define commandProcedure caddr)

(define-macro (define-command name&args documentation . body)
  (let ((name (car name&args))
	(args (cdr name&args)))
    `(begin (define ,name&args ,@body)
	    (addCommand ',name ',args ',documentation ,name))))

(define-command (-help)
  "Print help message"
  (display {[commandAppName] command*\n
With no commands an interactive prompt is provided.
command ::= -key arg*\n
Where command is one of:\n})
  (for-each (lambda (c)
	      (display (commandDocumentation c))
	      (newline))
	    (reverse commands)))

(define (commandProcessArgs args)
  ;; Process args and invoke commands.
  (cond ((null? args) 'done)
	((commandKeyword? (car args))
	 (collectCommandArgs (findCommand (car args)) (cdr args) '()))
	((.startsWith (car args) "(")
	 (jscheme.JS.eval (car args))
	 (commandProcessArgs (cdr args)))
	((not (.startsWith (car args) "-"))
	 (load (car args))
	 (commandProcessArgs (cdr args)))
	(else (display {Unknown argument: [(car args)]\n})
	      (-help))))

(define (collectCommandArgs command args soFar)
  ;; Accumulate the list of args for this command in soFar.
  ;; Apply the command to its arguments and continue processing.
  (cond ((or (null? args) (commandKeyword? (car args)))
	 (commandApply command (reverse soFar))
	 (commandProcessArgs args))
	(else (collectCommandArgs command
				  (cdr args)
				  (cons (car args) soFar)))))

(define (commandApply command args)
  (apply (commandProcedure command) args)
  (commandName command))
{</pre>
<hr>
<h2>Overwritable procedures</h2>
<p>Feel free to tailor these procedures for your application.
<pre>}
;;; Do something on startup.
(define (commandInit) #f)

(define (commandMain args)
  ;; The main program.
  ;; Parameter args is of type String[].
  (commandInit)
  (if (or (eq? args #null) (= (vector-length args) 0))
      (commandDefault)
      (begin
	(commandProcessArgs (vector->list args))
	(if commandExit? (commandExit))
	)))

(define (commandDefault)
  ;; This runs if there are no arguments.
  (Scheme.readEvalWriteLoop "> "))

;;; Should commandMain exit after processing all commands?
(define commandExit? #t)		

(define (commandExit)
  ;; What to do when commandMain exits.
  (System.exit 0))


