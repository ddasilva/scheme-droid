(import "java.io.File")
(import "java.util.zip.ZipFile")
(import "jsint.Symbol")

(use-module "elf/iterate.scm" 'import 'all)  ;; '(iterate crack map*))
(use-module "elf/util.scm" 'import 'all)     ;; '(isNull))

(define (apropos pattern . out)
  "Describe all global variables matching Pattern. Case insensitive"
  (let ((out (if (pair? out) (car out) (current-output-port)))
	(pattern (.toLowerCase (.toString pattern))))
    ;; Clone the table because getting a symbol's value can cause a
    ;; Java reflector to be installed.
    (iterate (.clone Symbol.symbolTable$)
	     (lambda (s)
	       (if (> (.indexOf (.toLowerCase (.toString s)) pattern) -1)
		   (display
		    {[s]: [(if (.isDefined s)
			       (let ((it (.getGlobalValue s)))
				 (if (isNull it) "null"
				     (.toString it)))
                               "")]\n}
		    out))))))

(define findClass
  ;; Example: (findClass "pattern")
  ;; Returns matching classes:  ((classname jar) ...)

  ;; To actually find a class instance from its full name use (class
  ;; full-name).
  (let ((classTable #f))
    (define (allJars)
      ;; Find all the jars in your application.
      (define (bootJars)
        (filter .exists
                (map File. (crack (System.getProperty "sun.boot.class.path")
                                  (System.getProperty "path.separator")))))
      (define (classLoaderJars cl)
        (let ((parent (.getParent cl))
              (paths (map File.
                          (filter (lambda (f)
                                    (and (or (.endsWith f ".jar")
                                             (.endsWith f ".zip"))
                                         (.exists (File. f))))
                                  (map* .getFile (.getURLs cl))))))
          (if (isNull parent) paths
              (append (classLoaderJars parent) paths))))
      (append (bootJars)
              (classLoaderJars (.getClassLoader jscheme.REPL.class))))
    (define (jarClasses file)
      ;; Returns a list of the names of classes in the .jar file, file.
      (define (classFromFile name)
        (apply string-append
               (separate "."
                         (crack (substring name 0 (- (string-length name)
                                                     (string-length ".class")))
                                "/"))))
      (map (lambda (_) (classFromFile (.getName _)))
           (filter (lambda (_) (.endsWith (.getName _) ".class"))
                   (.entries (ZipFile. file)))))
    (lambda (pattern)
      (if (not classTable)
          (begin
            (set! classTable
                  (apply append (map (lambda (j) (map (lambda (c) (list c j))
                                                      (jarClasses j)))
                                     (allJars))))
            (display {[(length classTable)] classes\n})))
      (filter (lambda (c) (!= (.indexOf (car c) pattern) -1)) classTable))))