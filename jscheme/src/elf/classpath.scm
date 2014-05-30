;;;
;;; Adaptive classpath
;;;

{
The class loader used by Jscheme can be accessed with
(Import.getClassLoader) and (Import.setClassLoader).

(replaceClassLoaders) creates a new class loader and resets Jscheme's
internal caches, so that classes can be recompiled and tested easily.
}

(use-module "elf/util.scm" 'import 'all)
(use-module "elf/iterate.scm" 'import 'all)

(import "java.net.URLClassLoader")
(import "java.net.URL")
(import "java.io.File")

(define-method (url (x String))
  ;; Return a URL
  (tryCatch (URL. x)
	    (lambda (e)
	      (or (and (instanceof e java.net.MalformedURLException.class)
		       (url (File. x)))
		  (throw e)))))

(define-method (url (url File)) (.toURL url))

(define-method (url (url URL)) url)

(define (replaceClassLoaders)
  (define (copyClassLoader loader)
    ;; Copy the chain of URLClassLoaders.
    (if (and (instanceof loader java.net.URLClassLoader.class)
             (not (eq? loader (.getClassLoader Import.class))))
        (URLClassLoader. (.getURLs loader) (copyClassLoader (.getParent loader)))
        loader))
  ;; Replace the classloader
  (Import.setClassLoader (copyClassLoader (Import.getClassLoader)))
  (.setContextClassLoader (Thread.currentThread) (Import.getClassLoader))
  (.clear Import.table$)
  (for-each* .reset Import.singles$)
  (for-each* .reset Import.wilds$)

  (.clear Invoke.constructorCache$)
  (.clear Invoke.staticCache$)
  (.clear Invoke.instanceCache$)
  (.clear JavaField.fieldTable$#)
  (.clear JavaField.fieldTablePriv$#)

  (jsint.Reflector.resetAll)

  (iterate
   (.elements (.clone Symbol.symbolTable$))
   (lambda (s) (if (and (.isDefined s)
			(instanceof (.getGlobalValue s) Class.class))
		   (.setGlobalValue s U.UNDEFINED$)))))

(define (path . args)
  ;; Convert arguments into a CLASSPATH or PATH string.
  (apply string-append
         (separate (System.getProperty "path.separator") (flatten args))))

(define (unpath path)
  ;; Convert a path string into a list of its component strings.
  (crack path (System.getProperty "path.separator")))

(define (classpathURLs) (.getURLs (Import.getClassLoader)))

(define (addClasspathUrl u)
  (.addURL# (Import.getClassLoader) (url u)))
