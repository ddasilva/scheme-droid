(use-module "jlib/JLIB.scm")
(use-module "gia_eval/giaeval.scm" 'import '(giaeval arithexprs))

;(jlib.JLIB.load)
;(import "net.sourceforge.interval.ia_math.RealInterval")

(define (main args)
  (define win (window "ia/scheme demo"))
  (init win)
  (.pack win) (.show win))



(define (init this)
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
              (label "General Interval Arithmetic Evaluator" (HelveticaBold 24))
              (label "Copyright Tim Hickey, January 2001, All Rights Reserved" (HelveticaItalic 10))
              (label "Enter an expression below and press the eval button.")))
        (center 
         (grid 2 1
           (tag "t" (textarea 5 20 white (CourierPlain 14)
                         "[2,4]/[-1,2)"       ))
           (splitcol 0.0 1.0
             (row
              (button "eval" (action
               (lambda (e) 
                  (writeexpr (tag "s")
                     (tryCatch
                           (let ((result (giaeval(readstring (tag "t")))))
                               (set! lastvalue result)
                                result)
                     (lambda(e) (list "Parsing Error" e)))))))
              (button "reset" (action (lambda(e) (set! iaenv ()))))
              (button "clear" (action (lambda(e)
                (.setText (tag "s") "") (.setText (tag "t") ""))))
              (tag "style" (choice "scale" "1" "2" "3" "4" "10" "20" "50" "100" "1000"
                (action (lambda(e)
                    (set! minscale (readexpr (tag "style")))
                    (set! maxscale (readexpr (tag "style")))))))

              (tag "demos" (apply choice (cons
                        (action (lambda(e)
                           (writestring (tag "t")
                               (second (assoc (readstring (tag "demos")) giademos)))))
                        (map first giademos))))

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
              (button "help" (action (lambda(e) (.show helpwin)))))
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
                 (card "syntax"
                    (textarea 10 40 yellow 
    "
                   SYNTAX
    The evaluator accepts any interval arithmetic expression and
    evaluates it to produce an interval which can be written
    using a variety of scales (i.e. digits after decimal point)
    in any of four notations (under the STYLE choice option).
    
    Arithmetic expressions are formed by applying operators from
    the previous help page to numbers and expressions. In particular,
    the interval specific notation is:
    
  1                                   point intervals
  (0,1)                               open intervals
  [0,1)                               half-open intervals
  [0,1]                               closed intervals
  [1.1,inf)                           unbounded intervals
  (-inf,inf)                          unbounded intervals
  {1,  [2,3),  (3,5],  (10,inf)}      unions of intervals, { I1, ... , In }

  Assignment Statements separated by semicolons:
    variable := expression ; ... ; expression
    a := (0,inf);
    a := 1/(1 + sq(a));
    b := 4;
    c := (a + 1/(1+b))*4;
    a+b+c

  Function Definitions:
   f(a,b,t) := b + t*(a-b);

  Redundant Function Definitions:
   f(a,b,t) := (b + t*(a-b)) in (t*a + (1-t)*b) ;

  Piecewise Function Definitions:
   f(a,b,t) :=
    {
      b+t*(a-b) if t in [-1,1];
      b if t in (-inf,-1);
      a if t in (1,inf)
    }


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
    Date:   1 January 2001
    
    The goal of this simple applet/application is to
    demonstrate the features of the interval arithmetic system
    described in the <a href=\"http://www.cs.brandeis.edu/~tim/Papers/iapi.ps.gz\">paper</a>
      Interval Arithmetic: from Principles to Implementation
      by T. Hickey, Q. Ju, and M.H. van Emden
    "                  ))

                )))
         (south (row (button "hide" (action (lambda(x) (.hide helpwin)))))))))
    

    (define (interval->string I)
       (ppS I))

    (define (RealInterval->string I)
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
  (set! jsint.BacktraceException.printJavaTrace$ #f)
  (.setLayout this (java.awt.GridLayout. 1 1))
  (.add this mainwindowpanel)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;; Sample interval arithmetic expressions to evaluate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
(define giademos '(
        ("[2,4]/[-1,2)" "[2,4]/[-1,2)" )
        ("1/sq(sq(sq(1.024)))" "1/sq(sq(sq(1.024)))")
        ("1/17" "1/17")
        ("[0,2)+(3,7]" "[0,2)+(3,7]")
        ("[0,2)*(3,7]" "[0,2)*(3,7]")
        ("[1,6]/[-inf,-3]" "[1,6]/[-inf,-3]")
        ("0/0" "0/0")
        ("1/0" "1/0")
        ("[0,1]/[0,1]" "[0,1]/[0,1]")
         ("unbounded intervals" 
             "a := (-inf,inf); b:=1+1/a; c:= 1/b")
;;;;;;;;;;;;;;;;
        ("abs fn" "
abs(x) := 
  {
    x if x in  [0,inf),
   -x if x in  (-inf,0)
  };
abs((-4,3])
                           ")
;;;;;;;;;;;;;;;;
      ("sign fn" "
sgn(x) :=
  {
    1 if x in (0,   inf),
    0 if x in [0,   0  ], 
   -1 if x in (-inf,0  ) };
sgn(1/(-2,3])
                             ")

;;;;;;;;;;;;;;;;
        ("e" "e := 
 1 + 1 *(1+1/2* (1+1/3* (1+1/4* (1+1/5* 
(1+1/6* (1+1/7* (1+1/8 *(1+1/9* (1+1/10*
(1+1/11*(1+1/12*(1+1/13*(1+1/14*(1+1/15*
(1+1/16*(1+1/17*(1+1/18*(1+1/19*(1+[1,3]/20)))))))))))))))))))")

;;;;;;;;;;;;;;;;
        ("exp version 1" "exp(x) := 
{
   1 + x *(1+x/2* (1+x/3* (1+x/4* (1+x/5* 
  (1+x/6* (1+x/7* (1+x/8 *(1+x/9* (1+x/10*
 (1+x/11*(1+x/12*(1+x/13*(1+x/14*(1+x/15*
 (1+x/16*(1+x/17*(1+x/18*(1+x/19*(1+[0.3,3]*x/20
                  )))))))))))))))))))
  if x in [0,0.125],

 sq(exp(x/2)) 
  if x in (0.125,inf), 

 1/exp(-x)  
  if x in [-inf,0)
};

exp(-16)
")

        ("exp version 2" "exp(x) := 
{
   1 + x *(1+x/2* (1+x/3* (1+x/4* (1+x/5* 
  (1+x/6* (1+x/7* (1+x/8 *(1+x/9* (1+x/10*
 (1+x/11*(1+x/12*(1+x/13*(1+x/14*(1+x/15*
 (1+x/16*(1+x/17*(1+x/18*(1+x/19*(1+[0,3]*x/20
                  )))))))))))))))))))
  if x in [-1,1],
(-inf,inf) if x in (1,inf),
(-inf,inf) if x in (-inf,-1)};

sq(sq(exp(1/4)))")

;;;;;;;;;;;;;;;;
        ("unions" "a := {[0,1),(2,3]}; 
b:= {(-inf,1],(100,101]};
c := a+b; d:=a-b;
e:= a*b;  f:= a/b;
g:= a/(1+a); h:= 1-1/(1+a)")
        ))

    
    
    
