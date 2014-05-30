{
 File:  startserver.scm 
 Author: Tim Hickey, adapting code from David May's jetty example
 Date: 7/28/2004

 Use: copy jscheme.jar and startserver.scm to the toplevel of the jetty folder
 and then issue the following command:
 % java -cp jscheme.jar jscheme.REPL startserver.scm

 This will start a server on port 8088.
}

(use-module "elf/basic.scm" 'import 'all)
(use-module "elf/classpath.scm" 'import 'all)
(use-module "using/run.scm" 'import 'all)

;;; Where the application's home directory is.
;;; This assumes that lib/jscheme.jar is the first thing in the classpath.
(define appDir
  (.getParentFile
   (.getParentFile
    (.getCanonicalFile
     (File. (car (crack ($ "java.class.path") ($ "path.separator"))))))))

(addClasspathUrl (File. appDir "src"))

; We load in all of the jars from the lib and ext folders
; we're actually only using 2 jars in lib/ and 2 in ext/ 
;  javax.servlet.jar, org.morthbay.jetty.jar, jasper-runtime.jar jasper-compiler.jar

;
;(for-each addClasspathUrl (files** (File. "lib") isJarFile))
(for-each addClasspathUrl (files** (File. "ext/webapp") isJarFile))
(for-each addClasspathUrl (files** (File. "ext/jetty") isJarFile))


(define (start-server port context webapp)
  (define server (org.mortbay.jetty.Server.))
  (define listener (org.mortbay.http.SocketListener.))
  (.setPort listener port);
  (.addListener server listener)
  (.addWebApplication server context webapp) 
  (.start server)
  server
)

; now startup two servers....
(define (start)
  (start-server 8088 "/jscheme" "."))

(define (gstart)
  (define lib (use-module "jlib/Swing.scm"))
  (define server #null)
  (define ta (textarea 10 40))
  (define w (window "JScheme server"
   (border
     (north (label "JScheme demo of Jetty webserver" (HelveticaBold 40)))
     (center ta)
     (south
      (col 'none 'center
       (button 
        "start server"
        (HelveticaBold 32)
        (color 230 240 255)
        (action 
         (lambda(e)
          (case (readstring (.getSource e))
           (("start server") 
               (set! server (start-server 8088 "/jscheme" "."))
               (appendstring ta {server started at [(Date.)]\n})
               (writestring (.getSource e) "stop server"))
           (("stop server") 
               (.stop server)
               (appendstring ta {server stopped at [(Date.)]\n})
               (writestring (.getSource e) "start server"))
           (else (display {unknown command: [(readstring (.getSource e))]\n}))
       ))))
     )))))
    
  (.pack w)
  (.show w)
)
