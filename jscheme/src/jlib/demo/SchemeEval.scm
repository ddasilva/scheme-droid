;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; 
;;  SchemeEval.scm
;; 
;;  author: Tim Hickey 
;;  date: 23 May 2002
;; 
;;  This is a GUI for the Jscheme interpreter. 
;;  It runs as a Java 1.0 applet.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
(jlib.JLIB.load)
 
(define (init this)  ;; create a GUI for the Scheme interpreter in the container "this"
 
    (define thisContainer  
      (tryCatch thisContainer (lambda(e) #null))) 
    
    (define ERRORS (java.io.StringWriter.))
    (define errorPort (java.io.PrintWriter. ERRORS))
    
  (define tag (maketagger))

  (define (getExprOnLine text line)
    (let loop ((L 1)(p 0))
      (if (> L line) 
          (.substring text p (.length text))
          (let ((pos (.indexOf text "\n" p)))
            (if (< pos 0)
                (.substring text p (.length text))
                (loop (+ L 1) (+ 1 pos)))))))


  ;; a read eval printloop from inport into the TextArea Ans 
  (define (REPloop Ans Text inport)  
   (begin 
     (define (loop x) 
      (if (eof-object? x) #t  
        (begin 
          (.appendText Ans 
            (string-append
                (jsint.U.stringify x)
                "\n--->\n" 
                (jsint.U.stringify 
                    (tryCatch (eval x (interaction-environment)) (lambda(e) e))) 
                   "\n\n"))
        (let ((line (+ 1 (.getLineNumber inport))))
         (tryCatch
            (loop (read inport))
            (lambda(e) (.appendText Ans {**** Syntax Error starting after expr on line [line]:\n\n [e]\n\n [(getExprOnLine Text line)]})
          )))
        )))
       (.setLength (.getBuffer ERRORS) 0)
       (loop (read inport))))
   


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

      (define Prog (textarea 8 40 (color 225 225 255) (Courier 12)))
      (define Ans (textarea 8 40 (color 200 255 200) (Courier 12)))
      (define MatchArea (textarea 6 40 (color 200 255 200) (Courier 12)))

      (define FontSizeField (textfield "12" 5
         (jsint.Listener11. (lambda(e) (.setFont Prog (java.awt.Font. "Courier" 0 (java.lang.Integer. (.getText FontSizeField))))))))
     
                                 ;; create the containers used in the mainPanel 
      (define statusbar (java.awt.Label. "                                                     "))


      (define DemoMenuBar 
         (menubar
           (menu "SchemeApplet"
             (menuitem "Quit"  (action (lambda (e) (.hide newwin)(.hide mainPanel) (.hide this))))

             (menu "view"
                (menuitem "show paren matcher"
                  (action (lambda(e)
			 (.show MatchArea)
			 (my-paren-matcher 'set 'indenting #t)
			 (my-paren-matcher 'set 'matching #t)
                         (.validate mainPanel))))

                (menuitem "hide paren matcher"
                  (action (lambda(e)
			 (.hide MatchArea)
			 (my-paren-matcher 'set 'indenting #f)
			 (my-paren-matcher 'set 'matching #f)
                         (.validate mainPanel)))))

             (menu "Font" "8" "10" "12" "14" "18" "24" "36" "48" "60" "120"
               (action (lambda(e) 
                  (let ((f (tryCatch
                              (java.awt.Font. "Courier" 0 (java.lang.Integer. (.getActionCommand e)))
                           (lambda (e) 12))))
                     (.setFont Prog f) (.setFont Ans f)(.setFont MatchArea f)))))
             (menu "Help"
               (menuitem "About" (action  (lambda (e) (.show aboutwindow))))
               (menuitem "Help"  (action (lambda (e) (.show helpwindow)))))))
        )

      (define current-eval-thread #null)
      (define EvalAction
        (action
         (lambda(e)  
            (set! current-eval-thread 
              (java.lang.Thread. (lambda()
                (.setText statusbar "Evaluating....")
                (REPloop Ans 
                   (.getText Prog)
                   (jsint.InputPort.  
                        (java.io.StringReader. (.getText Prog))))
                (.setBackground Prog (color 225 225 255))
                (.removeKeyListener Prog key-eater)
                (.setText statusbar "---ready---"))))
            (.addKeyListener Prog key-eater)
            (.setBackground Prog (color 225 255 225))
            (.start current-eval-thread))))

 
      (define helpwindow (textwindow "Help Window for Scheme Evaluator" 
      " 
      For help with the Jscheme language visit the
      Jscheme website:

         http://jscheme.sourceforge.net

      ")) 
       
      (define aboutwindow (textwindow "About SchemeEval.scm" 
      " 
      SchemeEval.scm 
       
      author: Tim Hickey 
      date: 5/22/2002 

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
          (button "Stop" (action (lambda(e)
            (.setBackground Prog (color 225 225 255))
            (.removeKeyListener Prog key-eater)
            (tryCatch (.stop current-eval-thread) (lambda(e) e)))))
          (button "Clear" (jsint.Listener11. (lambda (e) (.setText Prog "")(.setText MatchArea ""))))

          )))


      (define ansButtons
       (col 'horizontal 'north (grid 0 1 
          (button "Clear" (jsint.Listener11.
            (lambda (e) (.setText Ans "")(.setText MatchArea ""))))
      )))



      (define mainPanel           ;; this holds the interpreter, it can be moved from the applet to a Frame and back 
         (border 
           (north (border (center (label "Jscheme Interpreter" (TimesRomanBold 24))) 
                          (east Detach)  
                          (south statusbar)))
           (center (splitcol 0.6 0.4  
             (border (west progButtons) 
                     (center (border (center Prog) (south MatchArea))))
             (border (west ansButtons) (center Ans))))
      ))

     (define my-paren-matcher (make-paren-matcher Prog MatchArea))              
     (define key-eater (jsint.Listener11. (lambda(e) (.consume e))))
     (jsint.Scheme.setError errorPort)
     (.addKeyListener MatchArea (jsint.Listener11. (lambda(e) (.consume e))))
     (.addKeyListener Ans (jsint.Listener11. (lambda(e) (.consume e))))


     (tag "MatchArea" MatchArea)
     (tag "Prog" Prog)
     (tag "Ans" Ans)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PAREN MATCHING AND INDENTING
;; Here is the paren matching code
;; You invoke it by
;; (define z (make-paren-matcher P M))
;;   where P is the textarea containing the scheme code and
;;         M is the textarea in which the match info is displayed
;; You then activitate it using
;;   (z 'add-listener)
;; And you can deactivate it using
;;   (z 'remove-listener)
;; In the future we should be able to pass messages to z
;; to customize the indenting rules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-paren-matcher Prog MatchArea)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;  (find-start text)
    ;; this finds the stack of positions of all unclosed parentheses
    ;; from innermost to outermost.
    ;; But the stack begins with incode or
    ;; in-string P or
    ;; in-comment P
    ;; where P is the beginning of the string or comment
    ;; Also it throws an error if there are too many close parens
    ;; The error has the form:
    ;; (too-many-close-parens P)
    ;; where P is the location of the first extra close paren
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (find-start text)
      (define tokenizer (java.util.StringTokenizer. text "()\";\n\\" #t))
    
      (define (parse pos stack)
    ;    (display (list 'parse pos stack)) (newline)
        (if (not (.hasMoreElements tokenizer))
            (cons 'in-code stack)
            (parse-more pos (.nextElement tokenizer) stack)))
    
      (define (skip-comment pos stack)
        (if (not (.hasMoreElements tokenizer))
            (cons 'in-comment stack)
            (let* ((token (.nextElement tokenizer))
                   (newpos (+ pos (.length token))))
              (if (equal? token "\n") 
                  (parse newpos (rest stack))
                  (skip-comment newpos stack)))))
    
      (define (skip-quoted-char pos stack)
        (if (not (.hasMoreElements tokenizer))
            (skip-quoted-string pos stack)
            (let* ((token (.nextElement tokenizer))
                   (newpos (+ pos (.length token))))
              (skip-quoted-string newpos stack))))
    
      (define (skip-quoted-string afterquote? pos stack)
        (if (not (.hasMoreElements tokenizer))
            (cons 'in-string stack)
            (let* ((token (.nextElement tokenizer))
                   (newpos (+ pos (.length token))))
              (if (and (equal? token "\"")  (not afterquote?))
                  (parse newpos (rest stack)) 
                  (skip-quoted-string (equal? token "\\") newpos stack)))))
    
      ;; Note: when parsing a string or comment we push the position of " or ; onto the stack
      ;; This is sent back to the user if the program ends in a comment or string..
      (define (parse-more pos token stack)
    ;    (display (list 'parser-more pos token stack))(newline)
        (let ((newpos (+ (.length token) pos)))
          (cond 
           ((equal? token "(")  (parse newpos (cons pos stack)))
           ((equal? token ";")  (skip-comment newpos (cons pos stack)))
           ((equal? token "\"") (skip-quoted-string #f newpos (cons pos stack)))
           ((equal? token ")")  
            (if (null? stack) (throw (list 'too-many-close-parens newpos)) (parse newpos (rest stack))))
           (else (parse newpos stack)))))
    
      (parse 0 ()))
    
    
      ;; return a string of spaces representing the tab matching position z in the text
      (define (tabto z text) 
         (define (make-tab N)
           (java.lang.String.
              (list->array char.class 
                 (let loop ((s 1)(L ())) 
                    (if (>= s N) L 
                       (loop (+ s 1) (cons #' ' L)))))))
    
    
         (define (make-the-tab N)
           (let ((start (+ 1 (.lastIndexOf text "\n" z)))
                 (end (+ z N)))
             (make-tab (- end start))))
         (.toString
           (tryCatch
             (let ((z1 (+ 1 z)))
               (cond
                ((.startsWith text "(" z1) (make-the-tab 1))
                ((.startsWith text "if" z1) (make-the-tab 4))
                (else (make-the-tab 2))))
             (lambda(e) e))))
    
   ;; this listens for ENTER key presses, or for ")" keypresses
   ;; In the first case it indents the next line, 
   ;; in the second it displays the matching expression in the MatchArea
   ;; The variables (indenting, matching) are used to turn on/off these features
  (define my-key-listener
      (jsint.Listener11. (lambda(e) 
       (if (equal? (.getID e) java.awt.event.KeyEvent.KEY_RELEASED$)
        (tryCatch
         (let* ((text (.getText Prog))
		;; KRA 30MAY02: There is a Netscape 4 bug where each return is counted at 2 characters
		;; by the caret.
                (cpos (min (.getCaretPosition Prog) (.length text))))
	   ;; (display (list (.length text) cpos indenting matching))(newline)
	   ;; (write text) (newline)
          (cond 
                ((and
		  indenting
		  (equal? (.getKeyCode e) java.awt.event.KeyEvent.VK_ENTER$))
		 ;; (if (venerable-old-jvm?) (set! cpos (- cpos 1)))
                 (let ((z (tryCatch
			   (find-start (.substring text 0
						   (min cpos (.length text))))
				   (lambda(e) e))))
                   (case (first z)
                      ((too-many-close-parens) (.setCaretPosition Prog (second z)))
                      (else
                        (if (not (null? (rest z)))
                            (.insertText Prog {[(tabto (second z) text)] } cpos))))))

                ((and
		  matching
		  (equal? (.getKeyChar e) #\)))
                 (let ((z (tryCatch (find-start (.substring text 0 (+ -1 cpos))) (lambda(e) e))))
                   (case (first z)
                     ((too-many-close-parens)
                      (writeexpr MatchArea "too many close parentheses!"))
                     ((in-code)
                       (if (null? (rest z))
                           (writeexpr MatchArea "too many close parentheses!")
                           (writeexpr MatchArea {Last Complete Expression Typed:\n [
                               (tryCatch (.substring (.getText Prog) (second z) cpos) (lambda(e) {[
                                    (.toString (list (.length text) cpos z e))]}))]})))
                     (else #t)))
                   (.setCaretPosition MatchArea 0))
		
		;; KRA 30MAY02: Possible optimization: these 2 cases
		;; don't need the caret or the text.
                ((and
		  (or indenting matching)
		  (or
		   (equal? (.getKeyCode e) java.awt.event.KeyEvent.VK_DELETE$)
		   (equal? (.getKeyCode e) java.awt.event.KeyEvent.VK_BACK_SPACE$)))
                 (writeexpr MatchArea ""))

                 (else  #t 
;                     (writeexpr MatchArea {>>>[(Date.)]\n [
;                         (tryCatch (find-start (.substring text cpos))(lambda(e) e))]})
                    )))
        (lambda(e) (writeexpr MatchArea (list 'error-in-key-handler e))))))))

  (define matching #t)
  (define indenting #t)

  (.addKeyListener Prog my-key-listener)

  (lambda R
    (case (first R)
      ((set)
       (case (second R)
         ((matching) (set! matching (third R)))
         ((indenting) (set! indenting (third R)))))))

)

