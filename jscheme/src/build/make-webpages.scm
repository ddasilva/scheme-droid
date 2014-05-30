; run this by linking the jscheme folder into a webapp
; and visiting this servlet
;

(define (display-to-file filename data)
   (let (
         (f (java.io.PrintWriter. (java.io.FileWriter. filename #f)))
        )
      (.println f (jscheme.REPL.printToString  data #f))  ; don't quote the string ...
      (.close f)))


(define (read-string-from-file name default)
   (tryCatch
     (let* (
         (f (java.io.BufferedReader. (java.io.FileReader. name)))
         (data
          (let loop ((x (.readLine f)))
             (if (equal? x #null) ()
                 (cons (string-append x "\n") (loop (.readLine f)))))))
       (.close f)
       (apply string-append data))
     (lambda(e) default)))


 (define (generate-and-save the-context F)
   (display-to-file {[the-context]/[F].html} 
      (eval (string->expr (read-string-from-file {[the-context]/[F].servlet} "error"))))
   F)

(define (make-pages the-context) 
   (map 
    (lambda(x) 
      (display {generating [x]\n})
      (generate-and-save the-context x)
      )
    `(main 
      contrib/main 
      contrib/jswebapp/jswebapp
      contrib/ia/ia
      contrib/jswebapp/webxml
      contrib/jswebapp/install
      contrib/jswebapp/demo
      contrib/jswebapp/jscheme/demo/README
      doc/contributors
      doc/contributors/kra
      )))

(define (go) 

  (define libfiles 
    '(
					;  files.scm         ; (write-to-file F D) (append-to-file F D) (read-from-file F D) (read-all-from-file N D) 
					;(read-string-from-file N D) (servlet-file request N)
      forms.scm				; (make-form url . args)
      getformvalues.scm			; (get-form-values request x)
      servlet.scm			; (servlet vars . body) (basic-webpage title body) (warning-webpage body)
      session.scm			; (session-set request name value) (session-get request name default)
      tables.scm			; (ol L) (ul L) (lis L) (table Rows) (trs Rows)
      applets.scm			; (applet request width height file) (multi-jar-applet request W H F)
      redirect.scm			; (redirect response relativeurl)
      graphics.scm			; (send-jpg response w h drawfn)
      view.scm				; (viewCode file) 
					; mail.scm           ; (send-mail request to from subj text)
					; db.scm             ; database jdbc-driver jdcb-driver-class 
					;(connect host/db user pw) (handlequery con query) (runquery host/db user pw) (toMYSQL x) (toHSQL x) (toSQL x)
					; db.hide            ; (open-dbconnection) dbconnection dbquery
      ))
    
  (define ulibloaded 
    (map 
     (lambda(x) (load {contrib/jswebapp/jscheme/WEB-INF/scheme/servletlib/[(.toString x)]}))
     libfiles
     )
    )

  (load "src/build/initwebapp.servlet") ; load the page definitions
  (make-pages ".") 
  )
