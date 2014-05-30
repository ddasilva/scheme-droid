;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  JLIB.scm -- Java Library for Jscheme
;;
;;  This defines a high level GUI construction language 
;;  for writing applets in Jscheme. Key ideas:
;;
;;  * Flexible constructors for AWT components
;;      allow variable number of arguments and 
;;      generic methods to determine how to process
;;      the arguments, e.g.
;;        Font objects are used to set the font, ...
;;        Color objects set the background, ...
;;        Closures are passed the object and evaluated
;;        Listeners are handled as appropriate (e.g. add action listener....)
;;  * Flexible, declarative layout managers
;;      several layout managers are defined which
;;      allow for declarative gui construction, e.g.
;;        (row C1 ... Cn) creates a row
;;        (col C1 .... Cn) create a col
;;        (table Rows Cols A11 ... A1N A21 ... A2N ...) creates a table
;;  * Flexible component I/O 
;;      generic methods are used to define reading and
;;      writing of scheme terms and strings on components
;;        (readstring C) -- reads string from C
;;        (readexpr C) -- reads first expressions in C
;;        (readexprlist C) -- returns a list of all expressions in C
;;        (writeexpr T 23.5) -- writes the Scheme expressions 23.5 on T
;;        (appendlnexpr T A) -- appends the Scheme expression A on T
;;              and adds a newline.
;;  * Tagging
;;      the (maketagger) thunk creates a tagging procedure "tag"
;;      which can be used to name and lookup objects:
;;         (tag NAME object) stores object with key NAME in a hashtable
;;         (tag NAME) looks up the object with key NAME
;;  * Fonts/colors
;;      (CourierBold 48) == (java.awt.Font. "Courier" java.awt.Font.BOLD$ 28)
;;      red == java.awt.Color.red$
;;  * Error handling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(import "java.awt.*")
;(import "java.lang.*")
;(import "java.util.*")
;(import "jsint.*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Part I. JLIB constructors
;;    these are extensions of the standard Java constructors
;;    which allow multiple arguments processed in a standard
;;    way across all components.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  (class-case (a b c)
;;   ((Integer Object Double) .....)
;;   (( ... ) ...)
;;   ...
;;  )



(define-macro (class-case varlist . clauses)
   (define (runtimeClassName c)
     (string->symbol (string-append (.getName (class c)) ".class")))
   (define (instanceof v c) `(.isInstance ,(runtimeClassName c) ,v))
   `(cond ,@(map (lambda (clause)
                   (if (equal? (first clause) 'else) clause
                       `((and ,@(map instanceof varlist (first clause)))
                         ,@(rest clause))))

                 clauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic argument processing - key idea of JLIB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (processConArgs c as) 
    (define (processConArg c a)
      (class-case (c a)
        ((java.awt.Frame java.lang.String)
         (.setTitle c a))
        ((java.awt.MenuComponent java.lang.String)
         (writestring c a))
        ((java.awt.Component java.lang.String)
         (writestring c a))
        ((java.awt.TextField java.lang.Integer)
         (.setColumns c a))
        ((java.awt.TextArea jsint.Pair)
         (.setRows c (first a))
         (.setColumns c (second a)))
        ((java.awt.Frame java.awt.MenuBar)
         (.setMenuBar c a))
        ((java.awt.Container java.awt.LayoutManager)
         (.setLayoutManager c a))
        ((java.awt.Container java.awt.Component)
         (.add c a))
        ((jlib.SchemeCanvas java.awt.Image)
       (.paintHandler$ c (lambda(g)        (.setSize c (.getWidth a c) (.getHeight a c))       (.drawImage g a 0 0 c))))
        ((java.awt.Button jsint.JavaListener)
         (.addActionListener c a))
        ((java.awt.TextField jsint.JavaListener)
         (.addActionListener c a))
        ((java.awt.Choice jsint.JavaListener)
         (.addItemListener c a))
        ((java.awt.Checkbox jsint.JavaListener)
         (.addItemListener c a))
        ((java.awt.MenuItem jsint.JavaListener)
         (.addActionListener c a))
        ((java.awt.CheckboxMenuItem jsint.JavaListener)
         (.addItemListener c a))
        ((java.awt.MenuComponent java.awt.Color)
         (.setBackground c a))
        ((java.awt.MenuComponent jsint.Procedure)
         (a c))
        ((java.awt.MenuComponent java.awt.Font)
         (.setFont c a))
        ((java.awt.MenuBar java.awt.Menu)
         (.add c a))
        ((java.awt.Menu java.awt.MenuComponent)
         (.add c a))
        ((java.awt.Component java.awt.Color)
         (.setBackground c a))
        ((java.awt.Component jsint.Procedure)
         (a c))
        ((java.awt.Component java.awt.Font)
         (.setFont c a))
        ((java.awt.Component java.awt.Dimension)
         (.setSize c a))
        ((java.lang.Object java.lang.Object)
         (error (list "don't know how to process arg " a (.getClass a) " of component " c (.getClass c))))
       ))
  (for-each (lambda (x) (processConArg c x)) as)
  c
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define button (lambda R
  (let ((B (java.awt.Button. "")))
    (processConArgs B R))))
  
(define choice (lambda R
  (let ((C (java.awt.Choice.)))
    (processConArgs C R))))
  
(define label (lambda R
  (let ((L (java.awt.Label. "")))
    (processConArgs L R))))

(define textfield (lambda R
  (let ((T (java.awt.TextField. "" 20)))
  (processConArgs T R))))

(define textarea (lambda (r c . R)
  (let ((T (java.awt.TextArea. r c)))
  (processConArgs T R))))

(define textarea (lambda R ;; here we allow the deprecated use of textarea with two ints at the beginning
  (if (and (>= (length R) 2) (integer? (first R)) (integer? (second R)))
      (apply textarea (cons (list (first R) (second R)) (rest (rest R))))
      (let ((T (java.awt.TextArea. 10 50)))
          (processConArgs T R)))))


(define imagecanvas 
  (let* ((c (java.awt.Canvas.))
        (MT (java.awt.MediaTracker. c)))
    (lambda (C Name . R)
      (let ((I (if (.isInstance java.applet.Applet.class C)
                    (.getImage C (.getDocumentBase C) Name)
                    (.getImage (java.awt.Toolkit.getDefaultToolkit) Name))))
        (.addImage MT I 1)
        (.waitForAll MT)
        (let ((T (jlib.SchemeCanvas. (.getWidth I c) (.getHeight I c))))
          (.paintHandler$ T (lambda(g) (.drawImage g I 0 0 c) (.paintHandler$ T #null)))
          (processConArgs T R))))))

(define canvas (lambda (r c . R)
  (let ((T (jlib.SchemeCanvas. r c)))
  (processConArgs T R))))

(define (image c name)
 (class-case (c name)
   ((java.applet.Applet java.lang.String)
    (.getImage applet (.getDocumentBase applet) name))
   ((java.awt.Component java.lang.String)
    (.getImage (java.awt.Toolkit.getDefaultToolkit) name))))

(define window (lambda R
  (let ((win (java.awt.Frame.)))
    (.addWindowListener win (jsint.Listener11. (lambda(e) 
       (if (equal? (.getID e) java.awt.event.WindowEvent.WINDOW_CLOSING$)
           (.hide win)))))
    (.setLayout win (java.awt.GridLayout. 1 1))
    (processConArgs win R))))

(define checkboxgroup java.awt.CheckboxGroup.)

(define checkboxmenuitem (lambda R
  (define M (java.awt.CheckboxMenuItem.))
   (processConArgs M R)
   M))

(define menuitem (lambda R
  (define M (java.awt.MenuItem.))
   (processConArgs M R)
   M))

(define menu (lambda R
  (define M (java.awt.Menu. (first R)))
  (processConArgs M (map (lambda (x) (if (.isInstance String.class x) (menuitem x) x))(rest R)))
  M))

(define menubar (lambda R
  (define M (java.awt.MenuBar.))
  (processConArgs M R)
  M))

  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2. JLIB I/O
;;    Generic procedures for reading and writing strings 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (readstring Comp)
  (class-case (Comp)
     ((java.awt.Button) (.getLabel Comp))
     ((java.awt.MenuItem) (.getLabel Comp))
     ((java.awt.Choice) (.getSelectedItem Comp))
     ((java.awt.List) (.getSelectedItem Comp))
     ((java.awt.Label) (.getText Comp))
     ((java.awt.TextComponent) (.getText Comp))))


(define (writeOneString Comp Arg)
  (class-case (Comp)
    ((java.awt.Button)   (.setLabel Comp (.toString Arg)))
    ((java.awt.MenuItem) (.setLabel Comp (.toString Arg)))
    ((java.awt.Menu)     (.add      Comp (.toString Arg)))
    ((java.awt.Choice)   (.addItem  Comp (.toString Arg)))
    ((java.awt.List)     (.addItem  Comp (.toString Arg)))
    ((java.awt.Label)    (.setText  Comp (.toString Arg)))
    ((java.awt.TextComponent) (.setText  Comp (.toString Arg)))
  ))

(define writeexpr   (lambda (C . Args) (writeOneString C (apply string-append (map .toString Args)))))
(define writelnexpr (lambda (C . Args) (writeOneString C (apply string-append (map .toString (append Args (list "\n")))))))
(define appendexpr   (lambda (Comp . Args) (let  ((T (readstring Comp))) (apply writeexpr (cons Comp (cons T Args))))))
(define appendlnexpr (lambda (Comp . Args) (apply appendexpr (cons Comp (append Args (list "\n"))))))
(define readexpr     (lambda (Comp)        (string->expr (readstring Comp))))
(define readexprlist (lambda (Comp)        (string->exprlist (readstring Comp))))
(define writestring    writeexpr)
(define writelnstring  writelnexpr)
(define appendstring   appendexpr)
(define appendlnstring appendexpr)

(define expr->string jsint.U.stringify)
(define (string->expr x)  (read  (jsint.InputPort.  (java.io.StringReader. x))))
(define (string->exprlist x)
  (define (readlist port)
    (let ((a (read port)))
       (if (eof-object? a) ()
           (cons a (readlist port)))))
 (readlist (jsint.InputPort.  (java.io.StringReader. x))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LAYOUT CONSTRUCTORS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; border layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (center X) (list "Center" X))
(define (north X)  (list "North" X))
(define (south X)  (list "South" X))
(define (east X)   (list "East" X))
(define (west X)   (list "West" X))

;;   (border (list "Center" C) (list "North" N) color font ...)
(define border (lambda R
  (define b (java.awt.Panel.))
  (define (processBorderArgs R)
      (if (pair? (first R))
          (case (first (first R))
                (("Center" "North" "South" "East" "West") 
                 (.add b (first (first R)) (second (first R))) (processBorderArgs (rest R)))
                (else (error (list "Expected Center, North, South, East, or West "))))
          (processConArgs b R)))
  (.setLayout b (java.awt.BorderLayout.))
  (processBorderArgs R)
  b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define grid (lambda (r c . R)
  (let ((g (java.awt.Panel.)))
    (.setLayout g (java.awt.GridLayout. r c))
    (processConArgs g R)
)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variations on the grid bag layout constructor
;;
;; GridBagLayout allows one to use 11 values
;; to specify how a component should be placed
;; in the container. These values are stored
;; as a  GridBagConstraint.
;; The  fields are
;;   gridx gridy    int or RELATIVE
;;   gridwidth gridheight int or REMAINDER
;;   fill NONE BOTH HORIZONTAL VERTICAL
;;   ipadx ipady  int
;;   insets java.awt.Insets
;;   anchor CENTER NORTH NORTHEAST ...
;;   weightx, weighty double
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create a grid bag constraint with specified intial values for all fields
(define (gbc-constraint C gx gy gw gh f ipx ipy Is a wtx wty)
  (.gridx$ C gx)  (.gridy$ C gy)
  (.gridwidth$ C gw) (.gridheight$ C gh)
  (.fill$ C f)
  (.ipadx$ C ipx) (.ipady$ C ipy)
  (.insets$ C Is)
  (.anchor$ C a)
  (.weightx$ C wtx) (.weighty$ C wty)
  C)





(define (processGBCarg A C box gbc)
  (class-case (A)
    ((java.awt.Component)  (.setConstraints gbc A C) (.add box A))
    ((jsint.Symbol       )  (processSymbol A C))
    ((java.awt.Color    )  (.setBackground box A))
    ((jsint.Pair         )  (processPair (first A) (second A) C))
    ((java.lang.Object  )  (error "unrecognized box element " A))
  ))



(define (processSymbol S C)
      (case S
        ((both)       (.fill$ C java.awt.GridBagConstraints.BOTH$))
        ((none)       (.fill$ C java.awt.GridBagConstraints.NONE$))
        ((horizontal) (.fill$ C java.awt.GridBagConstraints.HORIZONTAL$))
        ((vertical)   (.fill$ C java.awt.GridBagConstraints.VERTICAL$))
        ((north)      (.anchor$ C java.awt.GridBagConstraints.NORTH$))
        ((northeast)  (.anchor$ C java.awt.GridBagConstraints.NORTHEAST$))
        ((northwest)  (.anchor$ C java.awt.GridBagConstraints.NORTHWEST$))
        ((south)      (.anchor$ C java.awt.GridBagConstraints.SOUTH$))
        ((southeast)  (.anchor$ C java.awt.GridBagConstraints.SOUTHEAST$))
        ((southwest)  (.anchor$ C java.awt.GridBagConstraints.SOUTHWEST$))
        ((center)     (.anchor$ C java.awt.GridBagConstraints.CENTER$))
        ((east)       (.anchor$ C java.awt.GridBagConstraints.EAST$))
        ((west)       (.anchor$ C java.awt.GridBagConstraints.WEST$))
        (else         (error "Unknown box attribute" (.toString S)))))



(define box (lambda R
  (define box (java.awt.Panel.))
  (define C (java.awt.GridBagConstraints.))
  (define gbc (java.awt.GridBagLayout.))


  (define (processPair name value C)
      (case name
        ((fill)     (.fill$ C value))
        ((anchor)   (.anchor$ C value))
        ((weightx)  (.weightx$ C value))
        ((weighty)  (.weighty$ C value))
             (else       (error "unrecognized box element " A))))

  (define (processGBCargs Args C box gbc)
    (if (null? Args) box
      (begin
         (processGBCarg (first Args) C box gbc)
         (processGBCargs (rest Args) C box gbc))))

  (define (processGBCtableargs type r c cols Args C box gbc)
    (if (equal? type 'cols)
        (begin    (.gridx$ C r)    (.gridy$ C c))
        (begin    (.gridx$ C c)    (.gridy$ C r)))
    (if (null? Args) box
      (let*
           ((A (first Args))
            (newcell (.isInstance java.awt.Component.class A))
            (newcol (and newcell (= c cols)))
            (c1 (if newcol 1 (if newcell (+ 1 c) c)))
            (r1 (if newcol (+ 1 r) r)))
         (processGBCarg (first Args) C box gbc)
         (processGBCtableargs type r1 c1 cols (rest Args) C box gbc))))

  (.setLayout box gbc)
  (gbc-constraint C
        (if (equal? (first R) 'row)   java.awt.GridBagConstraints.RELATIVE$ 1)
        (if (equal? (first R) 'row)   1 java.awt.GridBagConstraints.RELATIVE$)
        1 1 
        java.awt.GridBagConstraints.BOTH$ 1 1 
        (java.awt.Insets. 1 1 1 1) 
        java.awt.GridBagConstraints.CENTER$ 1.0 1.0)
   (case (first R)
      ((rows cols)
          (processGBCtableargs (first R) 1 1 (second R) (rest(rest R)) C box gbc))
      ((row col)
          (processGBCargs (rest R) C box gbc))
      (else
          (throw (list "ERROR in call to " `(box ,@R)))))))

(define row (lambda R (apply box (cons 'row R))))
(define table (lambda (r c . R) (apply box (cons 'rows (cons c R)))))
(define tablebyrow (lambda R (apply box (cons 'rows R))))
(define col (lambda R (apply box (cons 'col R))))
(define tablebycol (lambda R (apply box (cons 'cols R))))

(define (splitrow a1 a2 c1 c2)
  (define p (java.awt.Panel.))
  (define C (java.awt.GridBagConstraints.))
  (define gbc (java.awt.GridBagLayout.))
  (.setLayout p gbc)
  (gbc-constraint C 
        java.awt.GridBagConstraints.RELATIVE$ 1
        1 1 
        java.awt.GridBagConstraints.BOTH$ 1 1 
        (java.awt.Insets. 1 1 1 1) 
        java.awt.GridBagConstraints.CENTER$ (.doubleValue a1) 1.0)
  (.setConstraints gbc c1 C)
  (.add p c1)
  (.weightx$ C (.doubleValue a2))
  (.setConstraints gbc c2 C)
  (.add p c2)
  p
)

(define (splitcol a1 a2 c1 c2)
  (define p (java.awt.Panel.))
  (define C (java.awt.GridBagConstraints.))
  (define gbc (java.awt.GridBagLayout.))
  (.setLayout p gbc)
  (gbc-constraint C 
        1 java.awt.GridBagConstraints.RELATIVE$
        1 1 
        java.awt.GridBagConstraints.BOTH$ 1 1 
        (java.awt.Insets. 1 1 1 1) 
        java.awt.GridBagConstraints.CENTER$ 1.0 (.doubleValue a1))
  (.setConstraints gbc c1 C)
  (.add p c1)
  (.weighty$ C (.doubleValue a2))
  (.setConstraints gbc c2 C)
  (.add p c2)
  p
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Card layout
;;  usage:
;; (define mycards 
;;   (apply cards (map (lambda(x) (card (.toString x) (button (.toString x) (eval x))))
;;                               '(red green yellow blue orange cyan magenta white black))))
;; (define win (window "test" (col mycards 
;;       (apply row 
;;           (map (lambda (x) (button (.toString x) (action (lambda(e) (showcard mycards x))))) 
;;                (append '(first last next previous) 
;;                         (map .toString '(red green yellow blue orange cyan magenta white black))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cards (lambda R
  (define data (java.awt.Panel.))
  (define dataManager (java.awt.CardLayout.))
  (.setLayout data dataManager)
  (processConArgs data R)
  data))

(define (card name component)
  (lambda (this) (.add this component name)))

(define (showcard data which)
  (case which
     ((first)    (.first    (.getLayout data) data))
     ((last)     (.last     (.getLayout data) data))
     ((next)     (.next     (.getLayout data) data))
     ((previous) (.previous (.getLayout data) data))
     (else       (.show     (.getLayout data) data which))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts -- Fonts and Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (Courier N)       (java.awt.Font. "Courier" java.awt.Font.PLAIN$ N))
(define (CourierPlain N)  (java.awt.Font. "Courier" java.awt.Font.PLAIN$ N))
(define (CourierBold N)   (java.awt.Font. "Courier" java.awt.Font.BOLD$ N))
(define (CourierItalic N) (java.awt.Font. "Courier" java.awt.Font.ITALIC$ N))

(define (TimesRoman N)       (java.awt.Font. "TimesRoman" java.awt.Font.PLAIN$ N))
(define (TimesRomanPlain N)  (java.awt.Font. "TimesRoman" java.awt.Font.PLAIN$ N))
(define (TimesRomanBold N)   (java.awt.Font. "TimesRoman" java.awt.Font.BOLD$ N))
(define (TimesRomanItalic N) (java.awt.Font. "TimesRoman" java.awt.Font.ITALIC$ N))

(define (Helvetica N)       (java.awt.Font. "Helvetica" java.awt.Font.PLAIN$ N))
(define (HelveticaPlain N)  (java.awt.Font. "Helvetica" java.awt.Font.PLAIN$ N))
(define (HelveticaBold N)   (java.awt.Font. "Helvetica" java.awt.Font.BOLD$ N))   
(define (HelveticaItalic N) (java.awt.Font. "Helvetica" java.awt.Font.ITALIC$ N))

;; color shortcuts

(define white     java.awt.Color.white$)
(define lightGray java.awt.Color.lightGray$)
(define gray      java.awt.Color.gray$)
(define darkGray  java.awt.Color.darkGray$)
(define black     java.awt.Color.black$)
(define red       java.awt.Color.red$)
(define pink      java.awt.Color.pink$)
(define orange    java.awt.Color.orange$)
(define yellow    java.awt.Color.yellow$)
(define green     java.awt.Color.green$)
(define magenta   java.awt.Color.magenta$)
(define cyan      java.awt.Color.cyan$)
(define blue      java.awt.Color.blue$)
(define color     java.awt.Color.)

(define (size w h) (java.awt.Dimension. w h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tagging -- a simple interface to a hashtable
;;   (define tag (maketagger)) -- creates a hashtable associated to tag
;;   (tag "abc" OBJECT)  adds "abc"-OBJECT to tag hashtable and returns Object
;;   (tag "abc") looks up "abc" in hashtable to get corresponding object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maketagger)
  (let ((globals (java.util.Hashtable.)))
    (lambda (T . Vs)
      (let (
        (R (cond 
            ((null? Vs) 
             (.get globals T))
            ((and (pair? Vs) (null? (cdr Vs)))
             (.put globals T (first Vs))
             (first Vs))
            (else (throw `("improper use of \"tag\": " (tag  ,T  . ,Vs)))))))
       (if (eq? R #null)
          (throw (list "undefined tag " T Vs))
          R)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signalling Errors in a GUI interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define actionerrortextarea (textarea 20 80 white))
(define actionerrorwindow
  (let ((win (window "ERROR" 
        (border 
           (north (label "ERRORS generated during event handling" green (HelveticaBold 24)))
           (center actionerrortextarea)
           (south (button "clear" (jsint.Listener11. (lambda (e) (.setText actionerrortextarea "")))))))))
     (.setBackground win red)
     (.pack win)
     win))


(define error (lambda R
                (.println (.getError (jsint.Scheme.currentEvaluator))
                          (apply string-append (cons "JLIB USAGE ERROR:" (map jsint.U.stringify R))))))


(define (action p) (jsint.Listener11. (lambda (e) 
      (tryCatch (p e) (lambda (ee) 
           (appendlnstring actionerrortextarea 
               "\n\nERROR in Action in reponse to an AWT event" 
                e 
                " is " 
               ee)
           (.show actionerrorwindow)
           )))))

(set! jsint.BacktraceException.printJavaTrace$ #f)



