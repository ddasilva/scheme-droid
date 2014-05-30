(load "build/bootstrap.scm")
(jsint.Compile.load)
(let* ((bin (find-javac-bin))
       (javac (list (File. bin "javac") "-classpath" ".:../lib/servlet.jar"))
       (jar (File. bin "jar")))
  (and
   (run javac "-g" (files "jschemeweb" java-file?))
   (compile-file "jlib/SNLP.scm" "jlib")
   (run javac "-g" "jlib/SNLP.java")
   (run jar "cvf" "webapp/jscheme/WEB-INF/lib/jschemewebapp.jar" 
	(files "jschemeweb" class-file?)
	(files "jsint" class-file?)
	(files "jscheme" class-file?)
     )
   (run jar "cvf" "webapp/jscheme/lib/snlp.jar"
	(files "jlib" class-file?)
	(files "jsint" class-file?)
	(files "jscheme" class-file?)
     )
   (run jar "cvf" "webapp/jscheme/lib/applet.jar"
	(files "jlib" class-file?)
	(files "jsint" class-file?)
	(files "jscheme" class-file?)
     )
   (run jar "cvf" "../download/jschemewebapp.jar" "webapp/jscheme")
   (run jar "cvf" "../download/jschemewebapp.zip" "webapp/jscheme")
   (exit)
   ))

