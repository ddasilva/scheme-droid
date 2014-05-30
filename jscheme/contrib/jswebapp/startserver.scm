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

; first we load in all of the jars from the lib and ext folders
; we're actually only using 2 jars in lib/ and 2 in ext/ 
;  javax.servlet.jar, org.morthbay.jetty.jar, jasper-runtime.jar jasper-compiler.jar

(for-each addClasspathUrl (files** (File. "lib") isJarFile))
(for-each addClasspathUrl (files** (File. "ext") isJarFile))


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
(define server 
  (start-server 8088 "/jscheme" "/Users/tim/Desktop/MyJetty/webapps/jscheme"))
(define server2 
  (start-server 8090 "/" "/Users/tim/Research/Software/jscheme"))
