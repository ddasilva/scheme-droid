;; Fonts.scm -- a GUI for showing the available system fonts
;;
;; ** to run in the Demorunner, press "eval"
;;

(import "java.awt.*")
(import "javax.swing.*")

;; lookup font names
(define env (GraphicsEnvironment.getLocalGraphicsEnvironment))
(define fontnames (.GraphicsEnvironment.getAvailableFontFamilyNames env))

;; create the components
(define win (JFrame. "font demo"))
(define contents (.getContentPane win))
(define fonts (JComboBox. fontnames))
(define size (JTextField. "100" 5))
(define style (JComboBox. (list->array String.class '("plain" "bold" "italic"))))
(define ta (JTextArea. 5 10))
(.setText ta "Jscheme!")

;; layout the components
(define controls (JPanel.))
(.add controls fonts)
(.add controls size)
(.add controls style)
(.add contents controls BorderLayout.NORTH$)
(.add contents ta)

;; define/set the button/choice/textfield actions
(define resetfont (lambda(e)
  (.setFont ta (Font. (getFont fonts) (getStyle style) (getSize size)))))
(define resetfontListener (Listener11. resetfont))
(.addActionListener fonts resetfontListener)
(.addActionListener style resetfontListener)
(.addActionListener size  resetfontListener)
(define (getFont fonts)  (.getSelectedItem fonts))
(define (getSize size)  (Integer. (.getText size)))
(define (getStyle style) 
  (let ((s (.getSelectedItem style)))
    (cond ((equal? s "plain") Font.PLAIN$)
          ((equal? s "bold")  Font.BOLD$)
          ((equal? s "italic") Font.ITALIC$))))

;; quit when main window is closed
(.addWindowListener win (Listener11. (lambda (e)
  (if (.equals (.getID e) java.awt.event.WindowEvent.WINDOW_CLOSING$) 
      (tryCatch (System.exit 0)(lambda(e) #t))))))

;; initialize
(resetfont #null)


(.setSize win 600 200)
(.show win)

(define (main ShellArgs)  #t)
