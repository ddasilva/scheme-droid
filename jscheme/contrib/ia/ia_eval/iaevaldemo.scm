(use-module "jlib/JLIB.scm")
(use-module "ia_eval/iaeval.scm")

;(use-module "ia_eval/opparse.scm")
;(jlib.JLIB.load)
;         (load "opparse.scm")
;         (load "iaeval.scm")

(import "net.sourceforge.interval.ia_math.RealInterval")

(define (main args)
  (define win (window "ia/jscheme demo"))
  (init win)
  (.pack win) (.show win))


(define (init this)
  (define (loadFromApplet applet file)
       (jsint.Scheme.load 
          (jsint.InputPort. 
             (.openStream 
                (java.net.URL. 
                    (.getDocumentBase applet) 
                    file)))))

;  (if (.isInstance java.applet.Applet.class this)
;      (begin
;         (loadFromApplet this "opparse.scm")
;         (loadFromApplet this "iaeval.scm")
;      )
;      (begin
;         (load "opparse.scm")
;         (load "iaeval.scm")
;      ))
   (initIAeval this)
)
  

(define (initIAeval this)
    (define (show msg x) (display (list msg x)) (newline) x)
    
    (define stringlnappend (lambda R
      (apply string-append
         (let loop ((r R))
             (if (null? r) ()
                 (cons (first r) (cons "\n" (loop (cdr r)))))))))
    
    
    (define tag (maketagger))
    (define (lookup x L) (second (assoc x L)))
    
    (define lastvalue "")

    (define mainwindowpanel (grid 1 1 (java.awt.Color. 200 200 255) 
      (border
        (north 
            (col 
              (label "Interval Arithmetic Evaluator" (HelveticaBold 24))
              (label "Copyright Tim Hickey, June 2000, All Rights Reserved" (HelveticaItalic 10))
              (label "Enter an expression below and press the eval button.")))
        (center 
         (grid 2 1
           (tag "t" (textarea 5 20 white (CourierPlain 14)
                         "2^[0,1]*(2.718281828459045- (1 + 1/[10000,infinity])^10000)"
                    ))
           (splitcol 0.0 1.0
             (row
              (button "eval" (action
               (lambda (e) 
                  (writeexpr (tag "s")
                     (tryCatch
                        (interval->string
                           (let ((result (iaeval(readstring (tag "t")))))
                               (set! lastvalue result)
                                result))
                     (lambda(e) (list "Parsing Error" e)))))))
              (button "clear" (action (lambda(e)
                (.setText (tag "s") "") (.setText (tag "t") ""))))
              (tag "style" (choice "Style" "[lo,hi]" "mid +/- err" "relerror" "hex"
                (action (lambda(e)
                    (writeexpr (tag "s") (interval->string lastvalue))))))
              (tag "font" (choice "fontsize" "8" "10" "12" "14" "18" "24" "36" (action (lambda(e)
                   (if (equal? "fontsize" (readstring (tag "font"))) #f
                    (let ((F (CourierPlain (Integer. (readstring (tag "font"))))))
                      (.setFont (tag "t") F) (.setFont (tag "s") F)
                      (.validate this)))))))
              (tag "detach" (button "detach" 
                 (action (lambda(e) 
                   (.setEnabled (tag "attach") #t)(.setEnabled (tag "detach") #f)
                   (.add newwin mainwindowpanel) (.validate newwin)(.show newwin)))))
              (tag "attach" (button "attach" (lambda(this) (.setEnabled this #f))
                (action (lambda(e) 
                   (.setEnabled (tag "attach") #f)(.setEnabled (tag "detach") #t)
                   (.add this mainwindowpanel) (.validate this) (.hide newwin)))))
              (button "help/demos" (action (lambda(e) (.show helpwin)))))
            (tag "s" (textarea 5 20 white (CourierPlain 14)))))))))

    (define helpwin (window "Help Window"  (java.awt.Dimension. 600 400) (color 200 200 255)
      (border
         (north 
           (col 'none 'center 
             (row 'none 'center 
               (label "Help topics: " (HelveticaBold 24) )
               (tag "helpchoice"
                  (choice "demos" "syntax" "operators" "about" white (HelveticaBold 24) 
                     (action (lambda(e)
                       (showcard (tag "helpcards") (readstring (tag "helpchoice"))))))))))
         (center 
           (tag "helpcards"
              (cards

                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (card "demos"
                    (textarea 10 40 green
"
You can cut and paste these examples into the evaluation window

  [1,2]*[0.5,3]                       interval multiplication
  [0,1]/[1,infinity]                  infinity can appear only in interval bounds
  (1 + 1/[1000000,infinity])^[0,1000000]
  2^(1/[0,2])                         x^y denotes raising to a power, x must be positive
  exp([-infinity,0])                  elementary functions are computed exactly (not using Math.class)
  2^(52)*(1 - cos(355/113-4*atan(1))) this expression is only accurate to within about 0.5
  0/0                                 
"))


                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (card "syntax"
                    (textarea 10 40 yellow 
    "
                   SYNTAX
    The evaluator accepts any interval arithmetic expression and
    evaluates it to produce an interval which can be written
    in any of four notations (under the STYLE choice option).
    
       [lo, hi]
       mid +/- err
       mid*(1 +/- err)
       [lo_in_hex,hi_in_hex]
    
    Arithmetic expressions are formed by applying operators from
    the previous help page to numbers and expressions. In particular,
    the interval specific notation is:
    
      [A,B] returns an interval, the bounds can be infinite
            (i.e., A can be -infinity, B can be infinity).
    
      R(A,B) is an equivalent notation of intervals
    
    "                ))
                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (card "operators"
                    (tag "helpta" 
                       (textarea 10 40 pink
                          (apply string-append  
                           (cons 
                            
"
CURRENT OPERATORS RECOGNIZED BY INTERVAL ARITHMETIC PARSER.
Each operator is specified by 
  * its name,
  * its precedence (lower numbers bind tighter)
  * its associativity (xfy=right assoc infix, fx=prefix, xf=postfix)
  * its description
Some infix operators have an optional prefix specification
  (used for unary -)

token\tprecedence/associativity\tdescription

" 
                             (map (lambda(x) 
                                     (string-append 
                                        x
                                       "\n"))
                                   (lookup 'operators arithexprs)))))))

                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 (card "about"
                    (textarea 10 40 green
    "
    Author: Timothy J. Hickey
    Date:   14 July 2000
    
    The goal of this simple applet/application is to
    demonstrate two open source Java packages: 
    
     net.sourceforge.interval.ia_math  
              http://intervals.sourceforge.net

     jscheme     
              http://jscheme.sourceforge.net
    
    "                  ))

                )))
         (south (row (button "hide" (action (lambda(x) (.hide helpwin)))))))))
    
    (define (interval->string I)
     (cond 
       ((pair? I) (map interval->string I))
       ((not (.isInstance RealInterval.class I)) (.toString I))
       (else
          (case (readstring (tag "style"))
            (("Style" "[lo,hi]")
             (string-append "[" (.lo I) "," (.hi I) "]"))
            (("mid +/- err")
             (let ((lo (.lo I)) (hi (.hi I)))
               (let ((mid (/ (+ lo hi) 2))
                     (radius (/ (- hi lo) 2)))
                (string-append "(" mid "+/-" radius ")"))))
            (("relerror")
             (let ((lo (.lo I)) (hi (.hi I)))
               (let ((mid (/ (+ lo hi) 2))
                     (radius (/ (- hi lo) 2)))
                (string-append "(" mid " * (1 +/- " (/ radius (abs mid)) "))"))))
            (("hex")
              (string-append "(" (toHex (.lo I)) ", " (toHex (.hi I)) ")"))))))
    
    (define (toHex x)
      (string-append "0x"
      (java.lang.Long.toHexString (java.lang.Double.doubleToLongBits x))))
    
    (define lastparse ())
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Auxiliary functions
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (writelist S L)
      (if (null? L) ()
          (begin
              (appendlnexpr S (car L))
              (writelist S (cdr L)))))
  (define newwin (window "IAeval demo" (java.awt.Dimension. 400 400)))
  (.setLayout this (java.awt.GridLayout. 1 1))
  (.add this mainwindowpanel)
)
    
    
    
    
