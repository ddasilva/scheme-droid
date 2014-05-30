{
<html>
<head>
 <link rel="stylesheet" href="style.css">
<title>Run.scm</title>
</head><body>

<h1>run.scm</h1>

This is an extended version of the code presented at: <a
href="http://ll2.ai.mit.edu/talks/bbnll2/">http://ll2.ai.mit.edu/talks/bbnll2/</a>
that showed how 70 lines of code can compile a typical Java
application.  For example, the following function recompiles what's
needed under directory <tt>srcDir</tt>, putting <tt>.class</tt> files
in <tt>classDir</tt> using the current <tt>classpath</tt>.
<pre>
}
(define (compile srcDir classDir) 
  (let ((files (filter (needsUpdate? (java->class srcDir classDir))
		       (files** srcDir isJavaFile))))
    (if (> (length files) 0)
	(begin
	  (display {Compiling [(length files)] files.\n})
	  (out (run (cmd javac -sourcepath ,srcDir -d ,classDir
			 -classpath ,($ "java.class.path") ,files))))
	(display {No files need compiling.\n}))))
{
</pre>

This is similar to the Ruby example from <a href="http://www.ai.mit.edu/~gregs/ll1-discuss-archive-html/msg01937.html">Pixel</a>.

<p>This version provides documentation, and a <tt>run</tt> macro that
provides some of the capabilites of a UNIX shell to a Java
application.  For example, it provides for a PATH variable, a common
working directory, and piping external commands.  However, remember
that the only portable external commands are ones written in Java or
JScheme, such as <tt>java</tt>, <tt>javac</tt>, and <tt>jar</tt>.
<pre>
}

(import "java.io.*")
(import "java.net.URL")
(import "java.util.regex.Pattern")

(load "elf/basic.scm")

(define (url/file s)
  ;; Convert String or Symbol s into a URL or a File (if URL
  ;; conversion fails.
  (let ((s (.toString s)))
    (tryCatch (URL. s)
	      (lambda (e) (File. s)))))

;;; Construct a BufferedReader from various sources.
(define-method (BufferedReader (r Reader)) (BufferedReader. r))

(define-method (BufferedReader (s InputStream))
  (BufferedReader (InputStreamReader. s)))

(define-method (BufferedReader (f File))
  (BufferedReader (FileReader. f)))

(define ($ x . v)
  ;; ($ x)   - Get the system property x.
  ;; ($ x v) - Set the system property x to v.
  (if (null? v) (System.getProperty x)
      (System.setProperty x (car v))))

(define (contains target part)
  ;; Does string target contain the string part?
  (!= (.indexOf target part) -1))

{
</pre>
<h2>File searching</h2>

Procedures <tt>(files*)</tt>, <tt>(files**)</tt> and
<tt>(directories)</tt> return absolute Files.  
You can get files relative to a directory by using "r" prefixed versions.

<pre>
}
(define (isFile suffix)
  ;; Constructor for file predicates that check the suffix (file type).
  (lambda (file) (.endsWith (.toString file) suffix)))

(define isJavaFile   (isFile ".java"))
(define isClassFile  (isFile ".class"))
(define isSchemeFile (isFile ".scm"))
(define (isJarable file) (or (isClassFile file) (isSchemeFile file)))
(define isJarFile
  (let ((isJar (isFile ".jar"))
	(isZip (isFile ".zip")))
    (lambda (file) (or (isJar file) (isZip file)))))

(define-method (toFile (o Object)) (toFile (File. o)))
(define-method (toFile (o File)) o)

(define-method (iterate (x File) f)
  ;; Iterating on a directory iterates on its .listFiles.
  ;; Iterating on a normal File iterates on its lines.
  (if (.isDirectory x) (iterate (.listFiles x) f)
      (iterate (BufferedReader x) f)))

(define (files* directory type?)
  ;; Returns the files in directory that satisfy type?
  (let ((d (toFile directory)))
    (if (.isDirectory d) (filter type? (.listFiles d))
	'())))

(define (directories* directory)
  ;; Return the directories directly under directory.
  (files* directory .isDirectory))

(define (files** directory type?)
  ;; Returns a list of files under directory that satisfy type?.
  (append (files* directory type?)
	  (apply append (map (lambda (d) (files** d type?))
			     (directories* directory)))))

(define-method (relativize dir (f File))
  (File. (.substring (.toString f) (+ (.length (.toString dir)) 1))))

(define-method (relativize dir (fs Pair))
  (map (lambda (f) (relativize dir f)) fs))

(define (rfiles* d type?)  (relativize d (files* d type?)))
(define (rdirectories* d)  (relativize d (directories* d)))
(define (rfiles** d type?) (relativize d (files** d type?)))
{
</pre>
<h2>Making directories and copying files</h2>
<pre>
}
(define (mkdirs directory)
  ;; Make directory, directory.
  (.mkdirs directory)
  directory)

(define-method (copyBytes (in java.lang.Object) (out java.lang.Object))
  ;; Provide default N = 1001.
  (copyBytes in out 1001))

(define-method (copyBytes (in  InputStream) 
			  (out OutputStream) 
			  N)
  (let ((bs (make-array byte.class N)))
    (let loop ((i (.read in bs 0 N)))
      (if (not (= i -1)) (begin (.write out bs 0 i)
				(loop (.read in bs 0 N))))))
  (.close in)
  (.close out))

(define-method (copyBytes (in File) (out File) N)
  (copyBytes (FileInputStream. in) out N))

(define-method (copyBytes (in InputStream) (out File) N)
  (mkdirs (.getParentFile out))
  (copyBytes in (FileOutputStream. out) N))

(define-method (copyBytes (in URL) (out File) N)
  (copyBytes (.openStream in) out N))

(define (copyFiles from to files)
  (assert (not (eq? from to)))
  (mkdirs to)
  (for-each (lambda (f)
	      (copyBytes f (File. to (.toString (relativize from f)))))
	    files))
{
</pre>
There is a technical issue here.  Java on Windows seems to gatuitously
place several C:/WINNT directories at the beginning of The
java.library.path property.  So, for example, if you want to use the
cygwin find command you will actually run the windows one.

<p>On a Windows machine this code does its own lookup to get around
this problem putting windows commands at the end of the search list.
The simple definitions of PATH and (which) should work fine if you are
only invoking java related commands, such as javac and jar.

<pre>
}
(define PATH ($ "java.library.path"))
(define (which name) name)

(if (.startsWith ($ "os.name") "Windows")
    (begin
      (set! PATH
	    (let ((path (crack ($ "java.library.path") ($ "path.separator"))))
	      (append (filter (lambda (p) (not (contains p "WINNT"))) path)
		      (filter (lambda (p) (contains p "WINNT")) path))))
      (set! which
	    (lambda (name)
	      ;; Return the full pathname of the command name.
	      (if (.exists (File. name)) (File. name)
		  (let* ((name (if (not (.endsWith name ".exe")) {[name].exe}
				   name))
			 (result (filter (lambda (p) (.exists  (File. p name)))
					 PATH)))
		    (if (null? result) (error {Command [name] not found.})
			(File. (car result) name))))))))
{
</pre>
<h2>Current working directory</h2>
<pre>
}
  
(define (cwd)
  ;; Returns the current working directory.
  (File. ($ "user.dir")))

(define-method (cd (dir File))
  ;; Change the current working directory to be dir.
  ($ "user.dir" (.toString (.getCanonicalFile dir)))
  (cwd))

(define-method (cd (dir String)) (cd (File. dir)))
{
</pre>
<h2>Is file up to date?</h2>
<pre>
}

(define (s->o srcDir classDir fromtype totype)
  ;; Source file to object file converter.
  (lambda (file)
    ;; Converts a .java file in srcDir into a .class file in classDir.
    (let ((f (.toString (relativize srcDir file))))
      (File.  classDir
	      (string-append (.substring f 0
					 (- (.length f) (.length fromtype)))
			     totype)))))

(define (java->class srcDir classDir)
  (s->o srcDir classDir ".java" ".class"))

(define (needsUpdate? s->o)
  (lambda (jf)
    ;; Does .java file jf need to be recompiled?
    (let ((cf (s->o jf)))
      (or (not (.exists cf)) (<= (.lastModified cf) (.lastModified jf))))))

{
</pre>
<pre>
}
(define-macro (daemon . behavior)
  ;; A daemon is a daemon Thread that runs behavior.
  `(let ((it (Thread. (lambda () ,@behavior))))
     (.setDaemon it #t)
     it))

(define-macro (bg . behavior)
  ;; Run behavior in a background thread.
  `(.start (daemon ,@behavior)))

(define (drain r rc w wc)
  ;; Drain Reader r into Writer w.
  ;; Close the reader 
  ;; Close the writer if w
  (iterate r (lambda (L) (.println w L)))
  (if rc (.close r))
  (if wc (.close w)))

(define (out r . w)
  ;; If r is a Reader, drain it into w.
  ;; Otherwise, r is a process, drain its input into w and return
  ;; the process' status code.
  ;; The default value of w is System.out$
  (out0 r (if (null? w) System.out$ (car w))))

(define-method (out0 (r Reader) w)
  (drain r #t w (not (eq? w System.out$))))

(define-method (out0 (p Process) w)
  (out0 (inputReader p) w)
  (.waitFor p))
{
</pre>
<h2>Run command</h2>

<p>The <tt>(run command ...)macro takes one or more commands and
returns a Reader or a Process.  A command can have two forms:
<dl>

<dt>(in x)</dt><dd>Produces a Reader from <tt>x</tt>, that can be a
File, URL, or a String or Symbol naming a File or URL.  </dd>

<dt>(cmd arg ...)</dt><dd>Produces a Process
given the arguments<tt> arg ...</tt>.  The arguments are flattened and
converted into strings to make the real arguments used by the Process.
</dd> </dl>
A sequence of commands are treat as a pipe.
<p>
Run uses "," to unquote a expression to get its value in the Scheme
environment.  This trick was taught to me by Olin Shivers and he uses it in
<a href="http://sourceforge.net/projects/scsh/">scsh</a>.

Here are some example uses:
<ol>
<li>
To see how many lines of Java code you have in the
current working directory:

<pre>
}'
(out (run (cmd cat ,(files** (cwd) isJavaFile))
	  (cmd grep "[;}]")
	  (cmd wc -l)))
{
</pre>

<li>Make a .jar file of the .class and .scm files below the current
directory:

<pre>
}'
(out (run (cmd jar -cvf app.jar ,(rfiles** (cwd) isJarable))))
{
</pre>
<li>See the content of Google's main web page:

<pre>
}'
(out (run (in http://www.google.com)))
{
</pre>

<li>Sort your data and read it into a list of lines, in the background:

<pre>
}'
(define (readData file)
  (bg (set! data (map* identity (inputReader (run (cmd (sort ,file))))))))
{
</pre>
</ol>
One restriction imposed by Scheme's syntax is that '.' in a run
command must be in quotes:

<pre>
(out (run (cmd (find "." -name "'*.java'"))))
</pre>
Also, depending on the environment you are running in, you may need
to construct arguments specially, such as the "'*.java'" above.
<pre>
}

(define-macro (run . args) `(runRuntime ,(list 'quasiquote args)))
(define (runRuntime cmds) (runPipe #f cmds))

(define (runPipe previous cmds)
  ;; Thread command streams together, returning a BufferedReader, or Process.
  (if (null? cmds) previous
      (let* ((cmd (car cmds))
	     (cmds (cdr cmds))
	     (name (car cmd))
	     (args (cdr cmd)))
	(cond ((and (eq? name 'in) previous)
	       (error {[(.toString cmd)] must appear first.}))
	      ((eq? name 'in)
	       (runPipe (runIn (car args)) cmds))
	      (else (let ((p (runProcess args)))
		      (if previous
			  (bg (drain (if (instanceof previous Reader.class)
					 previous
					 (inputReader previous))
				     #t (outputWriter p) #t)))
		      (runPipe p cmds)))))))

(define (runProcess realargs)
  (let* ((args (map .toString (flatten realargs)))
	 (command (.toString (which (car args))))
	 (p (.exec (Runtime.getRuntime)
		   (list->array String.class (cons command (cdr args)))
		   #null		; Inherit from parent.
		   (cwd))))
    (bg (drain (errorReader p) #t System.out$ #f))
    p))

(define-method (runIn (b BufferedReader)) b)
(define-method (runIn (f File)) (BufferedReader f))
(define-method (runIn (s String)) (runIn (url/file s)))
(define-method (runIn (s Symbol)) (runIn (.toString s)))
(define-method (runIn (u URL)) (BufferedReader (.getContent u)))

(define (errorReader p)  (BufferedReader (.getErrorStream p)))
(define (inputReader p)  (BufferedReader (.getInputStream p)))
(define (outputWriter p) (PrintWriter. (.getOutputStream p)))

;;; Now this will work.
(define (compile srcDir classDir)
  (let ((files (filter (needsUpdate? (java->class srcDir classDir))
		       (files** srcDir isJavaFile))))
    (if (> (length files) 0)
	(begin
	  (display {Compiling [(length files)] files.\n})
	  (out (run (cmd javac -sourcepath ,srcDir -d ,classDir
			 -classpath ,($ "java.class.path") ,files))))
	(display {No files need compiling.\n}))))