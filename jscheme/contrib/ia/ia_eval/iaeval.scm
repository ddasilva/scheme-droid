;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  iaeval.scm
;;    Timothy J. Hickey Copyright 2000, All Rights Reserved
;;
;;  an interval arithmetic evaluator
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "net.sourceforge.interval.ia_math.IAMath");
(import "net.sourceforge.interval.ia_math.RealInterval");
(use-module "ia_eval/opparse.scm")

(define (iaeval-tree expr)

    (define (iaeval_expr op args)
     (case op
        (("infinity") (/ 1 0.0))
        (("pi") (ev "4*atan(1)"))
        (("e") (ev "exp(0)"))
        (("exp") (apply IAMath.exp (toInterval args)) )
        (("log") (apply IAMath.log (toInterval args)) )
        (("sin") (apply IAMath.sin (toInterval args)) )
        (("cos") (apply IAMath.cos (toInterval args)) )
        (("tan") (apply IAMath.tan (toInterval args)) )
        (("asin") (apply IAMath.asin (toInterval args)) )
        (("acos") (apply IAMath.acos (toInterval args)) )
        (("atan") (apply IAMath.atan (toInterval args)) )
        (("^") (apply IAMath.power (toInterval args)) )
        (("*") (apply IAMath.mul   (toInterval args)) )
        (("/") (apply IAMath.odiv  (toInterval args)) )
        (("+") (begin 
                 (case (length args) 
                    ((1) (first args))
                    ((2) (apply IAMath.add (toInterval args))))))
        (("-") (case (length args)
                  ((1) (if (number? (first args)) 
                           (- (.doubleValue (first args)))
                           (IAMath.uminus (first args))))
                  ((2) (apply IAMath.sub (toInterval args)))))
         ((",") (if (pair? (second args)) (apply cons args) (apply list args)))
         (("()") (first args))
         (("[]" "R" "Real")
             (let ((args (first args)))
                (cond ((and (number? (first args)) (number? (second args)))
                       (RealInterval. (.doubleValue (first args)) (.doubleValue(second args))))
                      (else 
                        (apply IAMath.union (toInterval (first args)))))))
    ;    (("abs") (apply IAMath.abs (toInterval args)) )
    ;    (("sqrt") (apply IAMath.sqrt (toInterval args)) )
    ;    (("sq") (apply IAMath.sq (toInterval args)) )
    ;    (("interval") (apply RealInterval. (toInterval args)) )
    ;    (("%") (apply IAMath.rem  (toInterval args)) )
         ((";") (if (pair? (second args)) (apply cons args) (apply list args)))
    ;    ((=) (apply IAMath.eq (toInterval args)) )
    ;    ((<) (apply IAMath.lt (toInterval args)) )
    ;    ((>) (apply IAMath.gt (toInterval args)) )
        (else (throw (list "error can't evaluate " op args)))))

    (define (toInterval X)
      (cond ((.isInstance RealInterval.class X) X)
            ((.isInstance java.lang.Number.class X) (RealInterval. (.doubleValue X)))
            ((pair? X) (cons (toInterval (first X)) (toInterval (rest X))))
            (else X)))
    
  (cond 
   ((string? expr) 
    (tryCatch 
       (java.lang.Double. expr)
       (lambda(e) (iaeval_expr expr ()))))
   ((pair? expr)   (iaeval_expr (first expr) (map iaeval-tree (rest expr))))
   (else (throw (list "unknown expression: " expr)))))



(define (iaeval s) (iaeval-tree (iaparse s)))
