{
  JSchemeEvaluator.scm
  Use: 
   As an applet, just add "app" to the suffix, i.e. view
     JSchemeEvaluator.scmapp
   As an application give the commmand
     java jscheme.REPL JSchemeEvaluator.scm '(make-solver-win)'
   Assuming you have jscheme.jar on your classpath  

}


(use-module "jlib/Swing.scm" 'import 'all)


; here is where we evaluate an expression in an interpreter
; and, in case of error, wereturn the entire error stack
; Change this if you want a different evaluation mechanism
(define (remote-eval js q)
     (Procedure.catching (lambda() (.eval js q)) (lambda(e) e)))

(define APPLET #f)
(define (install applet)
  (define SchemeEvaluator-win (window "JSchemeEvaluator"))
  (define ta-query (textarea 10 50 white (Courier 12)))
  (define ta-result (textarea 10 50 yellow (Courier 12)))
  (define button-detach
    (button "detach" (lambda(this) (.setEnabled this #f))(action (lambda(e) (detach-win)))))
  (define button-attach
    (button "attach" (lambda(this) (.setEnabled this #t)) (action (lambda(e) (attach-win)))))

  (define font-choice
    (choice "font" "12" "16" "18" "20" "24" "32" "40" "48" "60" "100" 
	    (action
	     (lambda(e) 
	       (tryCatch
                (let ((f1 (Courier (readexpr (.getSource e))))
                      (f2 (CourierBold (readexpr (.getSource e))))) 
		  (.setFont ta-query f1) (.setFont ta-result f2)
		  )
                (lambda(e) #t)
		)))))


  (define bgcolor (color 200 200 255))

  (define w 
    (border
     (north 
      (col bgcolor
	   (row 'none 'center bgcolor
		(row bgcolor
		     (label "JSchemeEvaluator 1.0 (23 July 2004)" (HelveticaBold 24) ))  (col bgcolor button-detach button-attach))
	   ))
     (center 
      (vsplit 0.5 
              (border
	       (west 
		(row 'none 'north bgcolor
		     (col'none 'north bgcolor
                         (button "eval" (action (lambda(e) (call-eval))))
                         font-choice
                         (button "clear all" (action (lambda(e) (clear-all))))
                         (button "reset" (action (lambda(e) (reset))))
			 )))
	       (center (scrollpane ta-query))
               )

              (border
	       (west 
		(col'none 'north  bgcolor
		    (row 'none 'west bgcolor
                         (button "clear" (action (lambda(e) (clear-result))))
			 )))
	       (center (scrollpane ta-result))
               )
	      )
      )
     )
    )

  (define (detach-win)
    (tryCatch (begin
		(.setEnabled button-attach #t)
		(.setEnabled button-detach #f)
		(.add (.getContentPane SchemeEvaluator-win) w)
		(.pack SchemeEvaluator-win)
		(.show SchemeEvaluator-win))
	      (lambda(e) (display (list 'error e)))))


  (define (attach-win)
    (.setEnabled button-attach #f)
    (.setEnabled button-detach #t)
    (.remove (.getContentPane SchemeEvaluator-win) w)
    (.setLayout applet (java.awt.GridLayout. 1 1))
    (.add applet w)
    (.hide SchemeEvaluator-win)
    (.validate applet)
    )

  (define js (jscheme.JScheme.))
  (define OUTPUT (java.io.StringWriter.))
  (define outputPort (java.io.PrintWriter. OUTPUT))
  (define (setPorts) 
    (let ((ev (.eval js '(Scheme.currentEvaluator))))
      (.setError ev outputPort)
      (.setOutput ev outputPort)))
  (define do-setPorts (setPorts))

  (define (call-eval)
    (define h (java.util.Hashtable.))
    (define q (readstring ta-query))
    (define (evalone q)
      (.eval js `(display (string-append "> " ',q)))
      (let ((result (remote-eval js q)))
	(.eval js `(display (string-append "\n"  ',result "\n\n")))))

    (define evalexprs
      (tryCatch 
       (begin
	 (display {exprs: [(.toString (string->exprlist q))]\n})
	 (map evalone (string->exprlist q))
	 (.append ta-result (.toString OUTPUT)))
       (lambda(e) 
	 (.append ta-result {SYNTAX ERROR in \n*********\n[q]\n**********\n is [e]\n\n})
	 (.append ta-result (.toString OUTPUT))
	 )))

    (.setLength (.getBuffer OUTPUT) 0)
    )

  (define (reset)
    (set! js (jscheme.JScheme.))
    (.setLength (.getBuffer OUTPUT) 0)  
    (setPorts))

  (define (clear-all)
    (writestring ta-result "")
    (writestring ta-query ""))
  (define (clear-result)
    (writestring ta-result ""))
  (define initial-panel 
    (col 'none 'northwest (color 0 0 100)
	 (button "show solver" 
		 (action (lambda(e) (.remove applet initial-panel) (attach-win))) 
		 )))
  ;; KRA 27AUG04: Make applet available so we can answer questions
  ;; about applet programming, as well as run Scheme.
  ;; The APPLET is either an Applet or a JPanel.
  (remote-eval js `(set! APPLET ,applet))
  (.setLayout applet (java.awt.GridLayout. 1 1 ))
  (.setBackground w bgcolor)
  (.setBackground applet bgcolor)
  (.setBackground SchemeEvaluator-win bgcolor)
  (attach-win)
;   (.add applet initial-panel)
  )

(define (make-solver-win)
  (define w (window "IASolver"))
  (install (.getContentPane w))
  (.pack w)
  (.show w))
