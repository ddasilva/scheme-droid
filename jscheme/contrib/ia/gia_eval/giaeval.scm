;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  giaeval.scm
;;    Timothy J. Hickey Copyright 2000, All Rights Reserved
;;
;;  an interval arithmetic evaluator for general intervals
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-module "op_parse/opparse.scm" 'import '(makeparser))
(use-module "gia_eval/ereal.scm")
(use-module "gia_eval/gia.scm")

; here is where we define the grammar of the language
(define arithexprs '(
  (delimiters "+*-/%^();:=<>,[]{} \t\n\r")
  (whitespace " \t\n\r")
  (multiparttokens 
        (":=" "<=" ">=" "!="))
  (outfix (
        ("("  openparen  "()"   "parentheses")
        (")"  closeparen "()"   "parentheses")
        ("["  openparen  "[]"   "brackets")
        ("]"  closeparen "[]"   "brackets")
        ("{"  openparen  "{}"   "braces")  
        ("}"  closeparen "{}"   "braces")  ))
  (operators (
;        ("abs"  200 fx "absolute value")
;        ("exp"  200 fx "exponential function")
;        ("log"  200 fx "natural logarithm")
;        ("sin"  200 fx "sine in radians")
;        ("cos"  200 fx "cosine in radians")
;        ("tan"  200 fx "tangent in radians")
;        ("asin" 200 fx "arcsin returns radians")
;        ("acos" 200 fx "arccos returns radians")
;        ("atan" 200 fx "arctangent returns radians")
;        ("sqrt" 200 fx "square root")
;        ("sq"   200 fx "square function")
;        ("R"    200 fx "real interval constructor")
;        ("^"    300 xfy "raise to real power")
        ("*"    400 yfx "multiplication")
        ("/"    400 yfx "division")
;        ("%"    400 yfx "remainder")
        ("+"    500 yfx "addition" 
                        unary-allowed ("+" 350 fy "unary addition"))
        ("-"    500 yfx "subtraction" 
                        unary-allowed ("-" 350 fy "unary subtraction"))
        ("in"    600 xfy "syntax for piecewise fn def")
        ("if"    600 xfy "syntax for peicewise fn def")
        (","    700 xfy "comma, to separate function arguments")
;        ("<"    800 xfx "less than predicate")
;        ("<="   800 xfx "less than or equal to predicate")
;        (">"    800 xfx "greater than predicate")
;        (">="   800 xfx "greater than or equal to predicate")
;        ("!="   800 xfx "not equal to predicate")
;        ("="    800 xfx "equality predicate")
        (":="   800 xfy "assignment")    
        (";"    900 yfx "expression terminator")

    ))
))


(define giaparse (makeparser arithexprs))

(define reals (s->S "{(-inf,inf)}"))

(define iaenv ())

(define (global-lookup x)
  (define (iter env)
     (cond ((null? env)
            (begin (set! iaenv (cons (list x reals) iaenv)) reals))
           ((equal? (first (first env)) x) (second (first env)))        
           (else (iter (rest env)))))
;  (display (list 'env iaenv))(newline)
  (iter iaenv))

(define (global-store x v) 
  (define (iter env)
;     (display (list 'iter env)) (newline)
     (if (null? env) 
         (begin (set! iaenv (cons (list x v) iaenv)) (list (list x v)))
         (if (equal? (first (first env)) x)
             (cons (list x v) (rest env))
             (cons (first env) (iter (rest env))))))
;  (display (list 'store iaenv x v)) (newline)
  (set! iaenv (iter iaenv)))

(define (lookup env x)
  (define (iter env)
     (cond ((null? env) (global-lookup x))
           ((equal? (first (first env)) x) (second (first env)))        
           (else (iter (rest env)))))
;  (display (list 'env iaenv))(newline)
  (iter env))

(define (store-vars Vars Vals env)
  (if (null? Vars) env
      (cons (list (first Vars) (first Vals)) (store-vars (rest Vars) (rest Vals) env))))

(define (store env x v) (cons (list x v) env))

(define fnenv ())

(define (fn-lookup x)
  (define (iter env)
     (cond ((null? env)
            (begin (set! fnenv (cons (list x reals) fnenv)) reals))
           ((equal? (first (first env)) x) (second (first env)))        
           (else (iter (rest env)))))
;  (display (list 'env fnenv))(newline)
  (iter fnenv))

(define (fn-store x v) 
  (define (iter env)
;     (display (list 'iter env)) (newline)
     (if (null? env) 
         (begin (set! fnenv (cons (list x v) fnenv)) (list (list x v)))
         (if (equal? (first (first env)) x)
             (cons (list x v) (rest env))
             (cons (first env) (iter (rest env))))))
;  (display (list 'store fnenv x v)) (newline)
  (set! fnenv (iter fnenv)))

(define (get-fn-vars L)
  (if (and (pair? L) (equal? (first L) ","))
     (cons (second L) (get-fn-vars (third L)))
     (list L)))

(define (iaeval-tree expr env)

    (define (iaeval_expr op args)
;     (display (list 'iaeval_expr op args))(newline)
     (case op
        ((error "error") 
         (if (equal? (first (first args)) 'OK) (second (first args)) (cons "error" args)))

         (("[]" "R" "Real")
             (let ((args (rest (first args))))
               (union0S (toInterval0 (first args)) (toInterval0 (second args)))))
;                (union (interval (first args) (second args) 
;                                 (isFinite (first args)) (isFinite (second args))))))

        (("parenmismatch [)") 
              (list 'OK (union (interval (second (first args)) (third (first args)) 
                                         (isFinite (second(first args))) #f))))
        (("parenmismatch (]") 
              (list 'OK (union (interval (second (first args)) (third (first args)) 
                                          #f (isFinite (third (first args)))))))
        (("()")  ;(display (list 'aaaaa (first (first args)) (first args) args)) (newline)
          (if (equal? 'union (first (first args)))
              (first args)
              (union (interval (second (first args)) (third (first args)) #f #f))
        ))

        (("{}") (unionS (map toInterval (rest (first args)))))

        (("*") (apply *S (map toInterval args)))
        (("/") (apply /S (map toInterval args)))
        (("+") (apply +S (map toInterval args)))
        (("-") 
          (cond ((= (length args) 2)
                 (apply -S (map toInterval args)))
                (else 
                  (if (pair? (first args)) 
                      (negateS (first args))
                      (negate (first args))))))
         ((",") (if (and (pair? (second args)) (equal? (first (second args)) ",")) 
                    `("," ,(first args) ,@(rest (second args)))
                    (cons "," args)))

         (("sq") (sqS (first args) ))
         (("in") (intersectS (first args) (second args)))
         ((";") (second args))
         (else 
            (let ((fndef (assoc op fnenv)))
               (if fndef
                  (begin ;(display fndef)(newline)
                      (if (all-non-empty args)
                          (iaeval-tree (second (second fndef)) 
                               (store-vars (first (second fndef)) args env))
                          '(union)))
                  (throw (list "error can not evaluate " op args)))))))
    
; (display (list 'iaeval-tree expr env)) (newline)
 (show (list "iaeval_expr" expr) 
  (begin
  ;(newline)(newline)  (display (list 'iaeval_expr expr))(newline)
  (cond 
   ((string? expr) (tryCatch (s->r expr) (lambda(e) 
                      (lookup env expr))))
   ((pair? expr)   
      (case (first expr) ;; first we handle the special forms
        ((":=") 
           (if (pair? (second expr))
               (begin 
                 ;(display (list 'assign (second expr) (third expr))) (newline)
                 (fn-store (second (second expr)) (list (get-fn-vars (second (third (second expr)))) (third expr)))
                 reals)
               (let ((r  (iaeval-tree (third expr) env))) 
                   ;(display (list 'assign (second expr) (third expr) r)) (newline)
                   (global-store (second expr) r)
                   (toInterval r))))
        ((?OP?) ;(display (list "OP" (first expr) (second expr) (third expr)))(newline) 
              (iaeval_expr (second expr) (map (lambda(x) (toInterval (iaeval-tree x env)))
                                              (get-fn-vars (second (third expr))))))
        (("if") 
             (let ((newdomain 
                   (iaeval-tree (list "in" (second (third expr)) (third (third expr))) env)))
               (if (is-empty newdomain) 
                   newdomain
                   (iaeval-tree (second expr) 
                                (cons (list (second (third expr)) newdomain)
                                       env)))))
        (else
         (iaeval_expr (first expr) (map (lambda(x) (iaeval-tree x env)) (rest expr) )))))
   (else (throw (list "unknown expression: " expr)))))))

(define (all-non-empty X)
  ;(display (list 'all-non-empty X))(newline)
  (if (null? X) #t
      (and (not (is-empty (first X))) (all-non-empty (rest X)))))
(define (is-empty X)
  (and (pair? X) (equal? (first X) 'union) (= (length X) 1)))

(define (show m x) ; (display (list m x))(newline) 
    x)

(define (giaeval s) 
  (tryCatch 
        (apply string-append
           `( "value of last expression:\n" ,(ppS (iaeval-tree (giaparse s) ())) "\n\nValues of Variables:\n"
              ,@(map (lambda(x) (string-append (first x) " = " (ppS (toInterval (second x))) "\n")) iaenv)))
                                                                     
     (lambda(e) 
        (if (.isInstance jscheme.SchemeThrowable.class e)
            (string-append "\nError in " s "\n " (.contents$ e))
            (string-append "ERROR while evaluating " s "\n" (.toString e))))))

(define (ia s) 
  ;(display (list 'ia 's= s 'iaenv= iaenv))(newline)(newline)
  (apply  string-append
    (cons 
       (ppS (giaeval s)) "\n\n"
       (map (lambda(x) (string-append (first x) " = " (ppS (second x)) "\n")) iaenv))))












