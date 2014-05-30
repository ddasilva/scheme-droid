{
 File: simpleserver.scm
 Author: Tim Hickey (adapting some code of David May)
 Date: 7/28/2004

 Starting a servlet directly in Jetty without a webapp.
 Here we show how to create two simple servlets running
 in a webserver....

 Use: 
   copy jscheme.jar and simpleserver.scm to the toplevel of the Jetty folder,  
   and execute the following command:
 % java -cp jscheme.jar jscheme.REPL simpleserver.scm

 This will start a server on port 8088
}

(use-module "elf/basic.scm" 'import 'all)
(use-module "elf/classpath.scm" 'import 'all)
(use-module "using/run.scm" 'import 'all)

; first we load in all of the jars from the lib and ext folders
; This example only needs two jars from the lib folder: javax.servlet.jar,  org.mortbay.jetty.jar
(for-each addClasspathUrl (files** (File. "lib") isJarFile))

; Next, create a server, a listener, and two ServletHttpContext (one for each servlet we will create)
;  (define server (org.mortbay.jetty.Server.))
  (define server (org.mortbay.http.HttpServer.))
  (define listener (org.mortbay.http.SocketListener.))
  (define time-servlet-context (org.mortbay.jetty.servlet.ServletHttpContext.))
  (define dump-servlet-context (org.mortbay.jetty.servlet.ServletHttpContext.))

; Now we can put in some SchemeServlets
  (define time-servlet-holder
     (.addServlet time-servlet-context "time-servlet" "*.time" "jschemeweb.SchemeServlet"))
  (define dump-servlet-holder
     (.addServlet dump-servlet-context "dump-servlet" "*.zz" "jschemeweb.SchemeServlet"))


; and define their functionality using Scheme code 
; (but don't initialize them until after the server is started!!)

  (define (init-time-servlet)
    (define the-servlet (.getServlet time-servlet-holder))
    (define do_get
      (lambda (request response)
        (.println (.getWriter response) {The local time is [(Date.)]\n\n})))
    (.do_get$ the-servlet do_get)
    (.do_put$ the-servlet do_get)
  )

  (define (init-dump-servlet)
    (define the-servlet (.getServlet dump-servlet-holder))
    (define do_get
      (lambda (request response)
        (.setContentType response "text/html")
        (.println (.getWriter response) 
         {The servlet parameters are: <ol>[
           (map* (lambda(x) {<li>[x] -> [(.getParameter request x)]</li>\n}) (.getParameterNames request))]</ol><br/>})))
    (.do_get$ the-servlet do_get)
    (.do_put$ the-servlet do_get)
  )




 (define start-it-up
  (begin
    (.setContextPath time-servlet-context "/js/*")
    (.setContextPath dump-servlet-context "/js/*")
    (.setPort listener 8088)
    (.addListener server listener)
    (.setContexts server 
        (list->array org.mortbay.jetty.servlet.ServletHttpContext.class
            (list
               time-servlet-context
               dump-servlet-context
            )))
    (.start server)
  ))


; Finally initialize the servlets
  (init-time-servlet)
  (init-dump-servlet)


