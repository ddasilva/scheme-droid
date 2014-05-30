; java -Djava.security.manager -Djava.security.policy=src/build/server.policy -classpath lib/jscheme.jar jscheme.REPL src/build/start-server.scm
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
;(define appDir
;  (.getParentFile
;   (.getParentFile
;    (.getCanonicalFile
;     (File. (car (crack ($ "java.class.path") ($ "path.separator"))))))))

;(addClasspathUrl (File. appDir "src"))

; We load in all of the jars from the lib and ext folders
; we're actually only using 2 jars in lib/ and 2 in ext/ 
;  javax.servlet.jar, org.morthbay.jetty.jar, jasper-runtime.jar jasper-compiler.jar

;
;(for-each addClasspathUrl (files** (File. "ext/webapp") isJarFile))
;(for-each addClasspathUrl (files** (File. "ext/jetty") isJarFile))


(define (start-server port context webapp)
  (define server (org.mortbay.jetty.Server.))
  (define listener (org.mortbay.http.SocketListener.))
  (display {setting port to [port]\n})
  (.setPort listener port);
  (display {adding listener to server\n})
  (.addListener server listener)
  (display {adding webapp "[webapp]" to server at context "[context]"\n})
  (.addWebApplication server context webapp) 
  (display {starting server\n})
  (.start server)
  (display {server started\n})
  server
)

; now startup two servers....
(define (start)
  (define webapp-dir (.getAbsolutePath (java.io.File. "contrib/jswebapp/jscheme")))
  (start-server 8088 "/jscheme/*" webapp-dir)
)

(define (gstart)
  (define lib (use-module "jlib/Swing.scm"))
  (define webapp-dir (.getAbsolutePath (java.io.File. "contrib/jswebapp/jscheme")))
  (define server #null)
  (define ta (textarea 10 40))
  (define port-tf (textfield "8088"))
  (define context-tf (textfield "/jscheme"))
  (define w (window "JScheme server"
   (border
     (north 
      (label
          {<html>
              <h1>JScheme demo of Jetty webserver</h1>
              This starts the contrib/jswebapp/jscheme webapp
           </html>}))
     (center ta)
     (south
      (col 'none 'center
       (table 2 2
         (label "port") port-tf
         (label "context") context-tf)
       (button 
        "start server"
        (HelveticaBold 18)
        (color 230 240 255)
        (action 
         (lambda(e)
          (case (readstring (.getSource e))
           (("start server") 
               (set! server (start-server (readexpr port-tf) (readstring context-tf) webapp-dir))
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

