(define (initialize httpservlet) 
  (define (getRealPath servlet File)
    (.getRealPath (.getServletContext (.getServletConfig servlet)) File))

  (define (fullname x) {WEB-INF/scheme/servletlib/[x]})
  (define (loadit x)
    (display {loading [(fullname x)] -- [(load (getRealPath httpservlet (fullname x)))]\n}))

 (map loadit '(      ; defines
  files.scm          ; (write-to-file F D) (append-to-file F D) (read-from-file F D) (read-all-from-file N D) 
                     ; (read-string-from-file N D) (servlet-file request N)
  forms.scm          ; (make-form url . args)
  getformvalues.scm  ; (get-form-values request x)
  servlet.scm        ; (servlet vars . body) (basic-webpage title body) (warning-webpage body)
  session.scm        ; (session-set request name value) (session-get request name default)
  tables.scm         ; (ol L) (ul L) (lis L) (table Rows) (trs Rows)
  applets.scm        ; (applet request width height file) (multi-jar-applet request W H F)
  redirect.scm       ; (redirect response relativeurl)
  graphics.scm       ; (send-jpg response w h drawfn)
  view.scm           ; (viewCode file) 
; mail.scm           ; (send-mail request to from subj text)
  db.scm             ; database jdbc-driver jdcb-driver-class (connect host/db user pw) (handlequery con query) 
                     ; (runquery host/db user pw) (toMYSQL x) (toHSQL x) (toSQL x)
  db.hide            ; (open-dbconnection) dbconnection dbquery
 ))
)

