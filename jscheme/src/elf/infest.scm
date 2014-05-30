{
If you start JScheme with something like this:

java -jar lib/jscheme.jar elf/infest.scm

Then loading this file will give your JScheme access to all the jars
under lib/.  Then you can play around and help someone debug their
Java application, just by adding jscheme.jar to their lib/.

}

(load "elf/basic.scm")
(load "elf/classpath.scm")
(load "using/run.scm")

;;; lib directory.
(define libDir (.getCanonicalFile
		(.getParentFile (File. ($ "java.class.path")))))

;;; Application home directory.
(define appDir (.getParentFile libDir))

;;; Extend classpath.
(for-each addClasspathUrl (files** libDir isJarFile))