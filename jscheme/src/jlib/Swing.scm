;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Swing.scm -- Swing Library for Jscheme
;;
;;  THE IDEA IS TO REWRITE THIS USING SWING INSTEAD OF THE AWT
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
;;        (tablebyrow N A11 ... A1N A21 ... A2N ...) creates a table
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
;(import "jscheme.*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Part I. Constructors
;;    these are extensions of the standard Java constructors
;;    which allow multiple arguments processed in a standard
;;    way across all components.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define swingcomp  (lambda (F . R)
  (for-each (lambda(x) (processConArgs F x)) R)
  F))


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
;; Generic argument processing - key idea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (processConArgs c as) 
    (define (processConArg c a)
      (class-case (c a)
        ((javax.swing.JFrame java.lang.String)
             (.setTitle c a))
        ((javax.swing.JMenu java.lang.String)
             (writestring c a))
        ((javax.swing.JMenuBar java.lang.String)
             (writestring c a))
        ((javax.swing.JMenuItem java.lang.String)
             (writestring c a))
        ((java.awt.Component java.lang.String)
             (writestring c a))
        ((javax.swing.JTextField java.lang.Integer)
             (.setColumns c a))
        ((javax.swing.JMenu java.awt.Color)
             (.setBackground c a))
        ((javax.swing.JMenuBar java.awt.Color)
             (.setBackground c a))
        ((javax.swing.JMenuItem java.awt.Color)
             (.setBackground c a))
        ((java.awt.Component java.awt.Color)
             (.setBackground c a))
        ((javax.swing.JMenu jscheme.SchemeProcedure)
             (a c))
        ((javax.swing.JMenuBar jscheme.SchemeProcedure)
             (a c))
        ((javax.swing.JMenuItem jscheme.SchemeProcedure)
             (a c))
        ((java.awt.Component jscheme.SchemeProcedure)
             (a c))
        ((javax.swing.JMenu java.awt.Font)
             (.setFont c a))
        ((javax.swing.JMenuBar java.awt.Font)
             (.setFont c a))
        ((javax.swing.JMenuItem java.awt.Font)
             (.setFont c a))
        ((java.awt.Component java.awt.Font)
             (.setFont c a))
        ((javax.swing.JTextArea jsint.Pair)
             (.setRows c (first a))
             (.setColumns c (second a)))
        ((java.awt.Component java.awt.Dimension)
             (.setSize c a))
        ((javax.swing.JFrame javax.swing.JMenuBar)
             (.setJMenuBar c a))
        ((java.awt.Container java.awt.LayoutManager)
             (.setLayoutManager c a))
        ((javax.swing.JFrame java.awt.Component)
             (.add (.getContentPane c) a))
        ((java.awt.Container java.awt.Component)
             (.add c a))
        ((javax.swing.JButton jsint.JavaListener)
             (.addActionListener c a))
        ((javax.swing.JTextField jsint.JavaListener)
             (.addActionListener c a))
        ((javax.swing.JComboBox jsint.JavaListener)
             (.addActionListener c a))
        ((javax.swing.JCheckBox jsint.JavaListener)
             (.addItemListener c a))
        ((javax.swing.JMenuItem jsint.JavaListener)
             (.addActionListener c a))
        ((javax.swing.JRadioButtonMenuItem jsint.JavaListener)
             (.addItemListener c a))
        ((javax.swing.JCheckBoxMenuItem jsint.JavaListener)
             (.addItemListener c a))
        ((javax.swing.JMenuBar javax.swing.JMenu)
             (.add c a))
        ((javax.swing.JMenu javax.swing.JMenu)
             (.add c a))
        ((javax.swing.JMenu javax.swing.JMenuItem)
             (.add c a))
        ((java.lang.Object java.lang.Object)
         (error (list "don't know how to process arg " a (.getClass a) " of component " c (.getClass c))))
  ))

  (for-each (lambda (x) (processConArg c x)) as)

  c
)


(define silencer (jsint.Listener11swing. (lambda(e) (.beep (java.awt.Toolkit.getDefaultToolkit)) (.consume e))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define button (lambda R
  (let ((B (javax.swing.JButton. "")))
    (processConArgs B R))))
  
(define choice (lambda R
  (let ((C (javax.swing.JComboBox.)))
    (processConArgs C R))))
  
(define label (lambda  R
  (let ((L (javax.swing.JLabel. "")))
    (processConArgs L R))))

(define textfield (lambda R
  (let ((T (javax.swing.JTextField. "" 20)))
  (processConArgs T R))))

(define textarea (lambda R ;; here we allow the deprecated use of textarea with two ints at the beginning
  (if (and (>= (length R) 2) (integer? (first R)) (integer? (second R)))
      (apply textarea (cons (list (first R) (second R)) (rest (rest R))))
      (let ((T (javax.swing.JTextArea. 10 50)))
          (processConArgs T R)))))

(define canvas (lambda (r c . R)
  (let ((T (jlib.SchemeCanvas. r c)))
  (processConArgs T R))))

(define (image comp name)
  (class-case (comp name)
     ((java.applet.Applet java.lang.String)
      (.getImage comp (.getDocumentBase comp) name))
     ((java.awt.Component java.lang.String)
      (.getImage (java.awt.Toolkit.getDefaultToolkit) name))
   ))

(define window (lambda R
  (let ((win (javax.swing.JFrame.)))
    (.setLayout (.getContentPane win) (java.awt.GridLayout. 1 1))
    (processConArgs win R))))

(define scrollpane (lambda (c . R)
  (let ((P (javax.swing.JScrollPane. c)))
    (processConArgs P R)
    P)))


(define buttongroup javax.swing.ButtonGroup.)

(define radiobuttonmenuitem (lambda R
  (define M (javax.swing.JRadioButtonMenuItem.))
   (processConArgs M R)
   M))

(define checkboxmenuitem (lambda R
  (define M (javax.swing.JCheckBoxMenuItem.))
   (processConArgs M R)
   M))

(define menuitem (lambda R
  (define M (javax.swing.JMenuItem.))
   (processConArgs M R)
   M))

(define menu (lambda R
  (define M (javax.swing.JMenu. (first R)))
  (processConArgs M (map (lambda (x) (if (.isInstance String.class x) (menuitem x) x))(rest R)))
  M))

(define menubar (lambda R
  (define M (javax.swing.JMenuBar.))
  (processConArgs M R)
  M))


(define dialog (lambda (parent modal title C)
  (let ((win (javax.swing.JDialog. parent modal)))
    (.setLayout (.getContentPane win) (java.awt.GridLayout. 1 1))
    (.setTitle win title)
    (.add (.getContentPane win) C)
    win)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2. I/O
;;    Generic procedures for reading and writing strings 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (readstring Comp)
  (class-case (Comp)
     ((javax.swing.JFrame) (.getTitle Comp))
     ((javax.swing.JButton) (.getLabel Comp))
     ((javax.swing.JMenuItem) (.getLabel Comp))
     ((javax.swing.JComboBox) (.getSelectedItem Comp))
     ((javax.swing.JList) (.getSelectedItem Comp))
     ((javax.swing.JLabel) (.getText Comp))
     ((javax.swing.JTextField) (.getText Comp))
     ((javax.swing.JTextArea) (.getText Comp))
     ((javax.swing.JEditorPane) (.getText Comp))
  ))

(define (writeOneString Comp Arg)
  (class-case (Comp)
    ((javax.swing.JFrame) (.setTitle Comp (.toString Arg)))
    ((javax.swing.JButton) (.setLabel Comp (.toString Arg)))
    ((javax.swing.JMenuItem) (.setLabel Comp (.toString Arg)))
    ((javax.swing.JMenu) (.add      Comp (.toString Arg)))
    ((javax.swing.JComboBox) (.addItem  Comp (.toString Arg)))
    ((javax.swing.JList) (.addItem  Comp (.toString Arg)))
    ((javax.swing.JLabel) (.setText  Comp (.toString Arg)))
    ((javax.swing.JTextField) (.setText  Comp (.toString Arg)))
    ((javax.swing.JTextArea) (.setText  Comp (.toString Arg)))
    ((javax.swing.JEditorPane) (.setText  Comp (.toString Arg)))
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
(define appendlnstring appendlnexpr)

(define expr->string jsint.U.stringify)


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
  (define b (javax.swing.JPanel.))
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
;;   (flow C1 C2 ... color font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow (lambda R
  (let ((g (javax.swing.JPanel.)))
    (.setLayout g (java.awt.FlowLayout.))
    (processConArgs g R)
    g
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   (nolayout (place C1 x y w h) ... color font)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define place (lambda R (cons 'place R)))

(define nolayout (lambda R
  (define g (javax.swing.JPanel.))
  (define (processNolayoutArgs R)
      (if (pair? (first R))
          (case (first (first R))
                ((place) 
                 (apply .setBounds (rest (first R)))
                 (.add g (second (first R)))
                 (processNolayoutArgs (rest R)))
                (else (error (list "Expected (place OBJ x y w h) "))))
          (processConArgs g R)))
  (.setLayout g #null)
  (processNolayoutArgs R)
  g
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grid constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define grid (lambda (r c . R)
  (let ((g (javax.swing.JPanel.)))
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
      ((java.awt.Component)   (.setConstraints gbc A C) (.add box A))
      ((jscheme.SchemeSymbol) (processSymbol A C))
      ((java.awt.Color)       (.setBackground box A))
      ((jscheme.SchemePair)   (processPair (first A) (second A) C))
      ((java.lang.Object)     (error "unrecognized box element " A))
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
  (define box (javax.swing.JPanel.))
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
(define tablebyrow (lambda R (apply box (cons 'rows R))))
(define col (lambda R (apply box (cons 'col R))))
(define tablebycol (lambda R (apply box (cons 'cols R))))
(define table (lambda (r c . R) (apply box (cons 'rows (cons c R)))))




;; this creates a JList. The first argument must be a callback
;; that is called when a single element of the list is selected.

(define jlist  (lambda (callback . args)
 (define this  (javax.swing.JList. #()))
 (processConArgs this args)
 (.addListSelectionListener this 
      (jsint.Listener11swing. (lambda(e) 
        (let ((val (.getSelectedValue this)))
             (if (not (equal? val #null))
              (if (.getValueIsAdjusting e) #t
                  (callback val)))))))
 this
))

;; this stores the list L of objects in the JList JL
(define (jlistwrite JL L)
  (let ((v (java.util.Vector.)))
    (for-each (lambda(x) (.addElement v x)) L)
    (.setListData JL v))
  (.validate JL))







(define (hsplit p A B)
 (let ((x (javax.swing.JSplitPane. javax.swing.JSplitPane.HORIZONTAL_SPLIT$ A B)))
    (.setOneTouchExpandable x #t)
    (.setDividerLocation x p)
    x))

(define (vsplit p A B)
 (let ((x (javax.swing.JSplitPane. javax.swing.JSplitPane.VERTICAL_SPLIT$ A B)))
    (.setOneTouchExpandable x #t)
    (.setDividerLocation x p)
    x))

(define hscrollpane (lambda (c . R)
  (let ((P (javax.swing.JScrollPane. c 
                 javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER$ 
                 javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS$)))
    (processConArgs P R)
    P)))

(define vscrollpane (lambda (c . R)
  (let ((P (javax.swing.JScrollPane. c 
                 javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS$
                 javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER$ )))
    (processConArgs P R)
    P)))



;; these ignore the a1 a2 parameters for now .....
(define (jsplitrow c1 c2)
  (define p (javax.swing.JSplitPane. javax.swing.JSplitPane.HORIZONTAL_SPLIT$ c1 c2))
  (.setOneTouchExpandable p #t)
  p)

(define (jsplitcol c1 c2)
  (define p (javax.swing.JSplitPane. javax.swing.JSplitPane.VERTICAL_SPLIT$ c1 c2))
  (.setOneTouchExpandable p #t)
  p
)


(define (splitrow a1 a2 c1 c2)
  (define p (javax.swing.JPanel.))
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
  (define p (javax.swing.JPanel.))
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
  (define data (javax.swing.JPanel.))
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
          (throw (list "undefined tag " T Vs ))
          R)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signalling Errors in a GUI interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define actionerrortextarea (textarea '(20 80) white))
(define actionerrorwindow
  (let ((win (window "ERROR" 
        (border 
           (north (label "ERRORS generated during event handling" green (HelveticaBold 24)))
           (center (scrollpane actionerrortextarea))
           (south (button "clear" (jsint.Listener11. (lambda (e) (.setText actionerrortextarea "")))))))))
     (.setBackground win red)
     (.pack win)
     win))

(define error (lambda R
  (.println (.getError (jsint.Scheme.currentEvaluator))
            (apply string-append
                   (cons "USAGE ERROR:" (map jsint.U.stringify R))))))

(define (action p) (jsint.Listener11. (lambda (e) 
      (tryCatch (p e) (lambda (ee) 
         (let ((msg (string-append
               "\n\nERROR in Action in reponse to an AWT event" 
                e 
                " is " 
               ee)))
           (display msg)(newline)
           (appendlnstring actionerrortextarea msg)
           (.show actionerrorwindow)
           ))))))


;(define (action p) (jsint.Listener. (lambda (e) 
;      (Trycatch (p e) (lambda (ee) 
;           (display
;               "\n\nERROR in Action in reponse to an AWT event" 
;                e 
;                " is " 
;               ee) (newline)(newline)
;           )))))

(set! jsint.BacktraceException.printJavaTrace$ #f)



