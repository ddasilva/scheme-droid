{
ia_solver.scm
author: Tim Hickey
date: 7/23/2004

USE:
  java -cp lib/jscheme.jar:lib/ia_math.jar jscheme.REPL ia_solver/ia_solver.scm '(make-solver-win)'
}

(import "net.sourceforge.interval.ia_math.RealInterval")
(import "net.sourceforge.interval.ia_math.IANarrow")

(use-module "jlib/Swing.scm" 'import 'all)
(use-module "elf/basic.scm" 'import '(map*))
(use-module "ia_solver/opparse.scm" 'import 'all "parse:")

(define (strip-parens expr)
  (if (string? expr) expr
      (if (equal? (first expr) "()")
          (strip-parens (second expr))
          (append (list (first expr)) (map strip-parens (rest expr))))))

(define (testread x)
  (define p (parse:makeparser parse:arithexprs))
  (strip-parens (p x)))

 ; syntree x hashtable -> contractor
 ; syntree is a syntax tree created by a parser
 ; H is a hashtable associating intervals to variables

(define (solve expr N)
  (solve expr N (java.util.Hashtable.)))

(define (solve expr N H)
  (define s (make-solver expr H))
  (let loop ((N N))
    (if (not (s 'contract))
        'unsatisfiable
        (if (> N 0) 
            (loop (- N 1))
            (s 'h)))))


  (define (make-solver expr)
    (make-solver expr (java.util.Hashtable.)))

  (define (make-solver expr h)
   (define p (testread expr))
   (define k (make-contractor p h))
   (lambda (msg)
     (case msg
      ((h) h)
      ((k) k)
      ((p) p)
      ((contract) (k 'contract)))))


  (define (make-contractor syntree H)
;    (display (list 'makingcontractor syntree H))(newline)
     (if (string? syntree)
         (process-leaf syntree H)
         (process-expression syntree H)))

  ; string x hashtable -> constraint
  (define (process-leaf leaf H)
     (let ((z (string->number leaf)))
       (if z 
          (make-number leaf z)
          (make-variable leaf H))))



  (define (process-expression syntree H)
    (make-complex-contraint syntree (map (lambda(x) (make-contractor x H)) (rest syntree))))

  ; string -> constraint
  (define (make-variable syntree H) 
    (if (equal? (.get H syntree) #null)
        (.put H syntree (RealInterval.)))
    (let ((val (.get H syntree)))
      (lambda (msg)
        (case msg
         ((type) 'variable)
         ((syntax) syntree)
         ((val) val)
         ((contract) #t)))))

  ; number -> constraint
  (define (make-number syntree num) 
    (let* 
      ((dnum (.doubleValue num))
       (val (RealInterval. dnum dnum)))
      (lambda (msg)
        (case msg
         ((type) 'number)
         ((syntax) syntree)
         ((val) val)
         ((contract) #t)))))

   (define (show-me msg x) (display (list msg x))(newline) x)

  ; syntree x args -> contractor
  (define  (make-complex-contraint syntree args)
    (let* ((op (first syntree))
           (val (RealInterval.))
           (vals (map (lambda(x) (x 'val)) args))
           (contract-me (lambda() (contract op val vals)))
           (contract-children (lambda() (map (lambda(x) (x 'contract)) args)))
         )
      (lambda(msg)
        (case msg
         ((type) 'expression)
         ((syntax) syntree)
         ((val) val)
         ((contract) 
          (if ia-solver-debugging 
            (begin (display (list "contracting " syntree " with vals= " vals )) (newline)))

          (let ((result
                  (and 
                     (contract-me)
                     (not (member #f (contract-children)))
                     (contract-me))))
            (if ia-solver-debugging 
               (begin (display (list result "is result of contracting " syntree " and vals = " vals))
                  (newline)(newline)
))
            result))
      ))))



(define ONE (RealInterval. 1.0 1.0))


(define ia-solver-debugging #f)

(define (contract op val  args)
  (case op
     (("+") (IANarrow.narrow_add val (first args) (second args) ))
     (("-") (if (equal? 2 (length args))
                (IANarrow.narrow_sub val (first args) (second args) )
                (IANarrow.narrow_uminus val (first args))))
     (("*") (IANarrow.narrow_mul val (first args) (second args)))
     (("/") (IANarrow.narrow_div val (first args) (second args)))
     (("exp") (IANarrow.narrow_exp    (first args) val))
     (("log") (IANarrow.narrow_log    (first args) val))
     (("sin") (IANarrow.narrow_sin    (first args) val))
     (("cos") (IANarrow.narrow_cos    (first args) val))
     (("tan") (IANarrow.narrow_tan    (first args) val))
     (("asin") (IANarrow.narrow_asin  (first args) val))
     (("acos") (IANarrow.narrow_acos  (first args) val))
     (("atan") (IANarrow.narrow_atan  (first args) val))
     (("^") (IANarrow.narrow_carot val (first args) (second args)))
     (("**") (IANarrow.narrow_power val (first args) (second args)))
     ((";") (IANarrow.narrow_semi val (first args) (second args)))
     ((":=") (IANarrow.narrow_colon_equals val (first args) (second args)))
     (("=") (IANarrow.narrow_equals (first args) (second args)))
     (("==") (IANarrow.narrow_eq val (first args) (second args) (third args)))
     (("<") (IANarrow.narrow_lt ONE (first args) (second args)))
     (("<=") (IANarrow.narrow_le ONE (first args) (second args)))
     ((">") (IANarrow.narrow_gt ONE (first args) (second args)))
     ((">=") (IANarrow.narrow_ge ONE (first args) (second args)))
     (("<>") (IANarrow.narrow_ne ONE (first args) (second args)))
     ((error "error") #t)
     (else (throw {error in ia-solver.scm: unknown operator [op]\n}))
    ))

(define tf-N (textfield "10" 10))
(define ch-style (choice "midpoint" "interval"))

(define (interval->string T I)
  (case T
    (("midpoint") (.toString I))
    (("interval") (.toString1# I))))

(define iasolver-win (window "IAsolver"))

(define (install applet)
  (define ta-query (textarea 10 50 yellow (Courier 12) "x^2+y^2=25;\n y=exp(x); x>0"))
  (define ta-result (textarea 10 50 (color 255 200 200) (Courier 12)))
  (define w 
    (border 
        (north (row 'none 'center (label "IASolver 2.0 (21 July 2004)" (HelveticaBold 32))))
        (south 
          (row 'none 'center 
           (table 2 2
            (label "num iterations") tf-N
            (label "interval-style") ch-style)))
        (center 
           (vsplit 0.5 
              (border
                  (west 
                     (row 'none 'north
                       (col'none 'north 
                         (button "solve" (action (lambda(e) (call-solver))))
                         (button "detach" (action (lambda(e) (detach-win))))
                         (button "attach" (action (lambda(e) (attach-win))))
                         (button "clear" (action (lambda(e) (clear-all))))
                       )))
                  (center (scrollpane ta-query))
               )

              (border
                  (west 
                     (row 'none 'north
                       (col'none 'north 
                         (button "clear" (action (lambda(e) (clear-result))))
                       )))
                  (center (scrollpane ta-result))
               )
            ))))

  (define (detach-win)
   (tryCatch (begin
    (.add (.getContentPane iasolver-win) w)
    (.pack iasolver-win)
    (.show iasolver-win)
    (.validate applet)
   )
   (lambda(e) (display (list 'error e)))))


  (define (attach-win)
;    (.remove (.getContentPane iasolver-win) w)
    (.hide iasolver-win)
    (.add applet w)
    (.validate applet)
  )


  (define (call-solver)
    (define h (java.util.Hashtable.))
    (define q (readstring ta-query))
    (define r (tryCatch (solve q (readexpr tf-N) h) (lambda(e) (list 'error e))))
    (cond
        ( (equal? r 'unsatisfiable)
          (.append ta-result "\n The system is not satisfiable!\n\n"))
        ( (pair? r)
          (.append ta-result {\nError: [(second r)]\n\n}))
        (else
	 (.append ta-result 
            {\n All solutions, if any, must lie in the following intervals:\n[(show-ans h)]\n\n}))))
  (define (clear-all)
    (writestring ta-result "")
    (writestring ta-query ""))
  (define (clear-result)
    (writestring ta-result ""))
  (define (show-ans h)
     (map* (lambda(x) {  [x]--> [(interval->string (readstring ch-style) (.get h x))]\n}) (.keys h)))

   (define initpane
         (row 'none 'northwest (col 'none 'northwest 
         (button "show solver" (action (lambda(e) (detach-win) (.remove applet initpane)))))))


   (.setBackground applet white)
   (.setLayout applet (java.awt.GridLayout. 1 1))
;   (.add applet initpane)
   (attach-win)
   (.validate applet)
 )

(define (make-solver-win)
  (define w (window "IASolver"))
  (install (.getContentPane w))
  (.pack w)
  (.show w))
