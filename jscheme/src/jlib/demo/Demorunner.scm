;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 
;;  Demorunner.scm
;; 
;;  author: Tim Hickey 
;;  date: 4 April 2002
;; 
;;  This is a GUI for the Jscheme interpreter. 
;;  It runs as a Java 1.0 applet and provides several demos that 
;;  can be selected from the "Demos" menu. 
;; 
;;
;; ** to run in the Demorunner, press "eval" and then "main" **
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
(jlib.JLIB.load)
 
(define thisContainer  
  (tryCatch thisContainer (lambda(e) #null))) 

(define ERRORS (java.io.StringWriter.))
(define errorPort (java.io.PrintWriter. ERRORS))

(define (init this)  ;; create a GUI for the Scheme interpreter in the container "this"
 
  (define tag (maketagger))

  ;; a read eval printloop from inport into the TextArea Ans 
  (define (REPloop Ans inport)  
   (begin 
     (define (loop x) 
      (if (eof-object? x) #t  
        (begin 
          (.appendText Ans 
            (string-append
                (jsint.U.stringify x)
                "\n--->\n" 
                (jsint.U.stringify 
                    (eval x)) 
                   "\n\n"))
          (loop (read inport))))) 
       (.setLength (.getBuffer ERRORS) 0)
       (loop (read inport)))) 
   


                                ;; determine where to look for input files 
  (define documentbase  
   (if (and (.isInstance java.applet.Applet.class this)
            (not (equal? (.getParameter this "DocumentBase") #null)))
     (java.net.URL. (.getDocumentBase this) (.getParameter this "DocumentBase"))
     (tryCatch (.getDocumentBase this) (lambda (e)    ;; from the webserver 
       (tryCatch (.getDocumentBase thisContainer) (lambda (e)    ;; from the interpreter applet! 
         (tryCatch                                          ;; or the user directory 
           (java.net.URL. 
              (.concat "file:" (.concat 
                    (java.lang.System.getProperty "user.dir") 
                     java.io.File.separator$ ))) (lambda (e) 
             (.println (.getError (jsint.Scheme.currentEvaluator)
                                  "ERROR trying to get a documentbase") 
             #null))))))))
          

  ;; read a file over the net and store in the TextArea Prog 
  (define (loadURL documentbase name Prog) 
     (tryCatch 
       (.setText Prog 
           (readStreamIntoString 
              (java.io.InputStreamReader. 
                (.openStream 
                   (java.net.URL. documentbase name))))) 
       (lambda(e) 
         (.appendText Prog (.toString (list "Error trying to load " name 
               " relative to the documentbase " documentbase))))) 
    )

    (define (readStreamIntoString stream) 
     (begin 
      (define reader  
          (java.io.BufferedReader. stream))
      (define (readloop sb) 
        (let ((x (.readLine reader))) 
          (if (equal? #null x) sb 
              (readloop  
                 (.append  
                     (.append sb x)  
                     "\n"))))) 
      (.toString 
         (readloop 
            (java.lang.StringBuffer. 8192)))))

 
 
 
  ;; Create an information window, used by "help" and "about" menuitems. 
  (define (textwindow Name T) 
   (let ((hw (window Name (color 255 205 155)))
         (ta (textarea 10 60 T)))
     (.setEditable ta #f) 
     (.add hw 
        (border 
          (center ta) 
          (south (button "OK" (action (lambda(e) (.hide hw)))))))
     (.pack hw) 
     hw))
 
 
 
 

  (define (createComponents) 
   (begin 
 
      (define newwin              ;; this is the Frame it will be moved from/to 
         (window "Jscheme interpreter frame")) 
                                  
                                  ;; create the components used in the mainPanel 

      (define Prog (jlib.EditArea. 8 40)) 
      (define Ans (java.awt.TextArea. 8 40)) 

      (define FileField 
        (textfield "" 30 (jsint.Listener11. 
          (lambda (e) 
               (loadURL documentbase (.getText FileField) Prog)))))


      (define FontSizeField (textfield "12" 5
         (jsint.Listener11. (lambda(e) (.setFont Prog (java.awt.Font. "Courier" 0 (java.lang.Integer. (.getText FontSizeField))))))))
     
                                 ;; create the containers used in the mainPanel 
      (define statusbar (java.awt.Label. "                                                     "))


      (define demolist '())

      (define (MenuAction Path)
              (action (lambda(e) 
                (.setText FileField (string-append Path (.getActionCommand e)))
                (.setText statusbar (string-append "loading " (.getText FileField)))
                (loadURL documentbase (.getText FileField) Prog)
;;                (.actionPerformed EvalAction e)
                (.setText statusbar (string-append (.getText FileField) " loaded"))
               )))


      (define DemoMenuBar 
         (menubar
           (menu "File"
              (menuitem "Open"  (action
                (lambda(e) 
                   (.setText statusbar "Opening file dialog box ...")
                   (.show FileDialogLoad) 
                   (.setText statusbar (string-append 
                         "loading " (.getFile FileDialogLoad) " ..."))
                   (let ((infile 
                     (java.io.FileReader. 
                        (java.io.File. 
                           (.getDirectory FileDialogLoad) 
                           (.getFile FileDialogLoad)))))
                   (.setText statusbar (string-append 
                         (.getFile FileDialogLoad) " loaded"))
               (.setText Prog
                 (readStreamIntoString infile))
               (.close infile)))))
             (menuitem "Save" (action
               (lambda(e) 
                   (.setText statusbar "Opening file dialog box ...")
                   (.show FileDialogSave) 
                   (.setText statusbar (string-append 
                         "saving " (.getFile FileDialogSave) " ..."))
                 (let ((chars (.getText Prog)) 
                   (outfile (java.io.FileWriter. 
                              (java.io.File.
                                 (.getDirectory FileDialogSave)
                                 (.getFile FileDialogSave)))))
             (.write outfile chars 0 (.length chars))
             (.setText statusbar (string-append 
                 (.getFile FileDialogLoad) " saved"))
             (.close outfile)))))
           (menuitem "Quit"  (action (lambda (e) (.hide newwin)(.hide mainPanel) (.hide this))))
          )          

            (menu "Font" "8" "10" "12" "14" "18" "24" "36" "48" "60" "120"
               (action (lambda(e) 
                  (let ((f (tryCatch
                              (java.awt.Font. "Courier" 0 (java.lang.Integer. (.getActionCommand e)))
                           (lambda (e) 12))))
                     (.appendText Ans (.toString e))
                     (.setFont Prog f) (.setFont Ans f)))))
                                     
         

;          (menu "Demos")
          (menu "Options"
           (menu "paren matching"    ;; Java 1.1 doesn't have checkboxgroups for menuitems
             (tag "flash P"
               (checkboxmenuitem "flash matching parens" 
                (lambda(this) (.setState this jlib.EditArea.matchParens$))
                (action (lambda(e)
                 (if (.getState (tag "flash P"))
                     (set! jlib.EditArea.matchParens$  #t)
                     (set! jlib.EditArea.matchParens$  #f))
             ))))
             (tag "show CE"
               (checkboxmenuitem "show current expr" 
                (lambda(this) (.setState this (not (equal? #null (.matchHandler$ Prog)))))
                (action (lambda(e)
                 (if (.getState (tag "show CE"))
                     (begin 
                         (set! jlib.EditArea.computeCurrentExpr$ #t)
                         (.matchHandler$ Prog showmatch))
                     (begin 
                         (set! jlib.EditArea.computeCurrentExpr$ #f)
                         (.matchHandler$ Prog #null)))
             ))))
            )
           (menu "bug workarounds"    ;; Java 1.1 doesn't have checkboxgroups for menuitems
             (tag "CR count bug"
               (checkboxmenuitem "Windows NT/Netscape/45.3" 
                (lambda(this) (.setState this jlib.EditArea.netscapebug$))
                (action (lambda(e)
                 (if (.getState (tag "CR count bug")) 
                     (set! jlib.EditArea.netscapebug$ #t)
                     (set! jlib.EditArea.netscapebug$ #f))))))
           )
          )
          (menu "Help"
           (menuitem "About" (action  (lambda (e) (.show aboutwindow))))
           (menuitem "Help"  (action (lambda (e) (.show helpwindow)))))
      ))


      (define Demos 
             (apply choice (append demolist (list 
                (jsint.Listener11.
         (lambda (e) 
              (begin 
                (.setText FileField (.getSelectedItem Demos)) 
                (.setText statusbar (string-append "loading " (.getText FileField) " ..."))
                (loadURL documentbase (.getText FileField) Prog)
;;                (.itemStateChanged EvalAction e)
                (.setText statusbar (string-append (.getText FileField) " loaded"))
                )))))))

      (define DemoWindow (window "Demos"
        (col (color 200 200 255)
          (row (label "Demo:") Demos)
          (row (label "File: ") FileField))))


      (define FileDialogLoad 
        (tryCatch (java.awt.FileDialog. DemoWindow "Jscheme File Loader" java.awt.FileDialog.LOAD$)
                  (lambda(e) #null)))
      (define FileDialogSave  
        (tryCatch (java.awt.FileDialog. DemoWindow "Jscheme File Saver" java.awt.FileDialog.SAVE$)
                  (lambda(e) #null)))

      (define EvalAction
        (action
         (lambda(e)  
                (.setText statusbar "Evaluating....")
                (REPloop Ans 
                   (jsint.InputPort.  
                        (java.io.StringReader. (.getText Prog))))
                (.setText statusbar "---ready---"))))

 
      (define helpwindow (textwindow "Help Window for Demorunner.scm" 
      " 
      Not all demos will run on all platforms.  
      In particular, the Swing demos only run 
      if you have a Java 1.2 appletrunner
      or a Java 1.1 runner which has the 
      Swing classes on its classpath.
      
      For help with the Jscheme language visit the
      Jscheme website:

         http://www.cs.brandeis.edu/~tim/jscheme/jscheme.html

      ")) 
       
      (define aboutwindow (textwindow "About Demorunner.scm" 
      " 
      Demorunner.scm 
       
      author: Tim Hickey 
      date: 4/5/2002 

      " 
      ))     
 
      (define Detach 
          (button "Detach" (jsint.Listener11.
           (lambda (e)  
            (if (equal? (.getLabel Detach) "Detach") 
              (begin 
                (.add newwin "Center" mainPanel) 
                (.hide this) 
                (.pack newwin) 
                (.show newwin) 
                (.setLabel Detach "Attach")) 
              (begin 
                (.add this "Center" mainPanel) 
                (.show this) 
                (.hide newwin) 
                (.validate this) 
                (.setLabel Detach "Detach")))))))

      (define progButtons 
        (col 'horizontal 'north 
         (grid 0 1 
          (button "Eval" EvalAction)
          (button "Main" (jsint.Listener11. (lambda (e) 
                (.setText statusbar (string-append "calling (main ()) ..."))
                (.appendText Ans (.toString (main ())))
                (.setText statusbar (string-append "---ready---"))
)))
          (button "Demo" (jsint.Listener11.
            (lambda (e) (.pack DemoWindow) (.show DemoWindow))))
          (button "Clear" (jsint.Listener11. (lambda (e) (.setText Prog ""))))
          )))


      (define ansButtons
       (col 'horizontal 'north (grid 0 1 
          (button "Clear" (jsint.Listener11.
            (lambda (e) (.setText Ans ""))))
      )))


      (define mainPanel           ;; this holds the interpreter, it can be moved from the applet to a Frame and back 
         (border 
           (north (border (center (label "Jscheme Interpreter" (TimesRomanBold 24))) 
                          (east Detach)
                          (south statusbar)))
           (center (splitcol 0.6 0.4  
             (border (west progButtons) 
                     (center Prog) )
             (border (west ansButtons) (center Ans))))
      ))

      (define showmatch
       (lambda (ea txt state)
         (.setText Ans 
            (string-append 
                (cond 
                   ((equal? state jlib.EditArea.inComment$) "\n**** in COMMENT ****\n")
                   ((equal? state jlib.EditArea.inQuote$) "\n**** in QUOTED STRING ****\n")
                   ((equal? state jlib.EditArea.tooManyParens$) "\n**** ERROR... TOO MANY CLOSE PARENS ****\n:")
                   ((equal? state jlib.EditArea.inExpr$) "current Expression:"))
                "\n"
                txt
                "\n----------------------------------------------------------------\n"
       ))))


      (jsint.Scheme.setError errorPort)
;      (.matchHandler$ Prog showmatch)

      (if (.isInstance java.applet.Applet.class this)
        (begin
          (.setMenuBar newwin DemoMenuBar)
          (.setLayout this (java.awt.BorderLayout.))
          (.add this "Center" mainPanel)
          (.setBackground mainPanel (color 200 200 255))
          (.setBackground Prog white)
          (.show mainPanel) 
          (.validate mainPanel) 
          (.validate this))
        (begin
          (.setMenuBar this DemoMenuBar)
          (.add this "Center" mainPanel) 
          (.pack this) 
          (.show this) 
          (.hide Detach))) 
       mainPanel 
   ))
   (set! thisContainer this) 
   (createComponents) 
)
 
 
(define (main ShellArgs)     ;; used by the Jscheme Compiler
     (define win 
            (window "Jscheme interpreter demo")) 
     (.setBackground win (color 200 200 255))
     (init win) 
     (.pack win) 
     (.show win))
