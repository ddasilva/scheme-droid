;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opparse.scm
;;     -- a Parser generator for Operator Precedence grammars
;;
;;  Timothy J. Hickey Copyright 2000, All Rights Reserved
;;
;;  This parser expects a specification of the grammar as shown below.
;;
;;  The only types of errors are missing parens, missing operators,
;;  and missing operands. The parser inserts error trees, or dummy 
;;  operators and operands, and continues parsing in all cases. 
;;  The resulting output can be useul in reporting syntax errors.
;;
;;  The goal of this package is to provide a general operator precedence
;;  parser which translates infix/prefix/postfix/outfix operator grammars
;;  into scheme syntax trees where they can be conveniently processed in scheme.
;;  The parser is designed to automatically insert ?op? operators and ?arg?
;;  operands when needed so that it can complete a parse. Mismatched parens
;;  however generate error expressions.
;;
;;  For expressions E without errors the output is an sexpression
;;  where the leaves are strings and the internal nodes have the form
;;     E = (F E1 ... En)
;;  where F is an operator or outfix symbol pair (e.g. "()" "[]" ...)
;;  and the operands Ei are either internal nodes or leaves.
;;
;;  Use: 
;;    > (define iaparse (makeparser arithexprs))
;;    > (iaparse "sin(1+[2,3])^(1/2)")
;;
;;        ("^" ("sin" ("()" ("+" "1" ("[]" ("," "2" "3"))))) ("()" ("/" "1" "2")))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; All specifications have the form
;;
;;  ((delimiters STRING) 
;;   (whitespace STRING) 
;;   (outfix ((SYM openparen/closeparen PAIR DESCRIPTION) ...))
;;   (operators ((SYM PREC ASSOC DESCRIPTION) ...)))
;;
;;  Some infix operators can further provide an overloaded prefix specification
;;  (to handle unary -)
;;
;; Below is a sample grammar specification for arithmetic expressions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is a sample arithmetic expression specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is the definition of the parser
;; This is what we import into a scheme program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (makeparser spec)
  ;; unpack fields from the grammar specification
  (define (lookup x L) (second (assoc x L)))
  (define delimiters (lookup 'delimiters spec))
  (define whitespace (lookup 'whitespace spec))
  (define outfix (lookup 'outfix spec))
  (define operators (lookup 'operators spec))
  (define multiparttokens (lookup 'multiparttokens spec))

  (define (isWhiteSpace? T) (<= 0 (.indexOf whitespace T)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Lexical analysis
  ;; tokenizer extends java.util.StringTokenizer
  ;;   * it removes whitespace
  ;;   * it returns multipart tokens (e.g. := from : and =)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (tokenizer string)
     (list ()
         (java.util.StringTokenizer. string (lookup 'delimiters spec) #t)))


  (define (pushtokenback token tokenizer)
     (set-car! tokenizer (cons token (first tokenizer))))

  ;; skips white space and pushes token back if there is one
  (define (skipWhiteSpace tokenizer)
    (if (and (null? (first tokenizer)) 
             (not (.hasMoreTokens (second tokenizer)))) #f
    (if (not (null? (first tokenizer))) #t
        (let ((x (.nextToken (second tokenizer))))
           (if (isWhiteSpace? x)
               (skipWhiteSpace tokenizer)
               (pushtokenback x tokenizer))))))

  (define (noMoreTokens tokenizer)
     (skipWhiteSpace tokenizer)
     (and (null? (first tokenizer))
          (not (.hasMoreTokens (second tokenizer)))))

  (define (getNextToken tokenizer)
    (skipWhiteSpace tokenizer)
    (let ((T (first (first tokenizer))))
      (set-car! tokenizer (rest (first tokenizer)))
      T))

  (define (getNextFullToken tokenizer)
      (define (startsOperator prefix tokenlist)
          (cond ((null? tokenlist) #f)
                ((.startsWith (first tokenlist) prefix) #t)
                (else (startsOperator prefix (rest tokenlist)))))
      (define (try-to-extend prefixes additionaltokens tokenizer) 
         (cond ((or
                   (noMoreTokens tokenizer)
                   (not (startsOperator (first prefixes) multiparttokens)))
                (find-longest-token prefixes additionaltokens tokenizer))
               (else
                (let ((T (getNextToken tokenizer)))
                  (try-to-extend
                        (cons (string-append (first prefixes) T) prefixes) 
                        (cons T additionaltokens)
                        tokenizer)))))
      (define (find-longest-token prefixes additionaltokens tokenizer)
          (if (or (member (first prefixes) multiparttokens)  
                  (null? additionaltokens))
              (first prefixes)
              (begin
                  (pushtokenback (first additionaltokens) tokenizer)
                  (find-longest-token (rest prefixes) (rest additionaltokens) tokenizer))))
      (define (pushbacktokens Ts tokenizer)
           (if (null? Ts) #t
               (begin
                  (pushtokenback (first Ts) tokenizer)
                  (pushbacktokens (rest Ts) tokenizer))))
    
      (let ((T (getNextToken tokenizer)))
         (try-to-extend (list T) () tokenizer)))


  ;; There are four types of tokens created by the "classify" constructor
  (define (getType token) (first token))
  (define (operator?   token) (equal? (getType token) 'op))
  (define (openparen?  token) (equal? (getType token) 'openparen))
  (define (closeparen? token) (equal? (getType token) 'closeparen))
  (define (operand?    token) (equal? (getType token) 'term))

  (define (classify T)
      (let ((Operator (assoc T operators))
            (Outfix (assoc T outfix)))
        (cond (Operator 
                 (list 'op Operator))           ; 'op is for all infix, prefix, postfix ops
              (Outfix     
                 (list (second Outfix) Outfix)) ; of type 'openparen or 'closeparen
              (else 
                 (list 'term T)))))



  ;; access function for operands
  (define (getContents Term) (second Term))

  ;; access functions for other tokens.
  (define (getOpProps op) (second op))
  (define (getName op) (first (getOpProps op)))
  (define (getPrec op) (second (getOpProps op)))
  (define (getAssoc op) (third (getOpProps op)))
  (define (getParenType  paren) 
    (third (getOpProps paren)))

  (define (unary-allowed? op) 
     (and
        (> (length (getOpProps op)) 4)
        (equal? (list-ref (getOpProps op) 4) 'unary-allowed)))
  (define (getUnaryAlt op) (list-ref (getOpProps op) 5))

  (define (postfix? token) (member (getAssoc token) '(xf yf)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Syntactic Analysis -- simple SR parsing,
  ;; extended to allow multiple paren matching (i.e. outfix ops)
  ;; and simple error detection/reporting
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (srparser tokenizer stack)
     (define (srhandler token stack)
;        (display (list 'srhandler token stack))(newline)
        (cond 
          ((and     ;; missing operand error: add ?arg? operand and continue
                (or (operator? token) (closeparen? token))    
                (or (null? stack) (operator? (first stack)) (openparen? (first stack))))
           (srhandler 
                (overload-test token)
                (cons '(term ?arg?) stack)))

          ((shift? token stack)
           (srparser tokenizer (cons token stack)))

          ((reduce? token stack)
           (srhandler token (reducestack stack)))

          (else    ;; this case should not happen
             `((term (error (in_srhandler ,tokenizer ,stack)))))))

     (if (noMoreTokens tokenizer)
       (let ((stack (if (operator? (first stack)) (cons '(term ?arg?) stack) stack)))
         (let loop ((stack stack))
             (if (> (length stack) 1) 
                 (loop (reducestack stack))
                 (if (equal? 'term (first (first stack))) (second (first stack))
                     `(error (in_srparser ,stack))))))
       (srhandler (classify (getNextFullToken tokenizer)) stack)))


  (define (shift? token stack)
    (or
       (<= (length stack) 1)
       (operand? token)
       (openparen? token)
       (operator? (first stack))
       (and 
         (not (operand? (second stack)))
          (< (windowprecedence token) 
             (stackprecedence (second stack))))))

  (define (reduce? token stack) #t)


  ;; define precedence relations for SR parsing
  (define maxprec 1000000.0)
  (define minprec -1000000.0)

  (define (windowprecedence token) 
     (if (closeparen? token) 
         maxprec
     (if (openparen? token)   ;; this only happens in error cases
         minprec
         (let ((p (getPrec token))
               (type (getAssoc token)))
           (case type 
             ((fx xfx xf) (* 2 p))
             ((fy xfy) (- (* 2 p) 1))
             ((yf yfx) (* 2 p))
             (else            ;; this should never happen
                (throw (list "error_in_windowprecedence" token))))))))
         

  (define (stackprecedence token) 
     (cond
       ((openparen? token) 
         (+ 1 maxprec))
       ((closeparen? token)  ;; this only happens in error cases
         minprec)
       ((postfix? token)
         minprec)
       (else
         (let ((p (getPrec token))
               (type (getAssoc token)))
           (case type 
             ((fx xfx xf) (* 2 p))
             ((fy xfy) (* 2 p))
             ((yf yfx) (- (* 2 p) 1))
             (else             ;; this should never happen
                (throw (list "error_in_stackprecedence" token))))))))



  ;; stack reduction operation -- always succeeds (although may need to insert missing operator ?op?
  (define (reducestack stack)
    (if (closeparen? (first stack))
        (reduceparens stack)
    (if (closeparen? (second stack))
        (cons (first stack) (reduceparens (cdr stack)))
    (if (openparen? (second stack))
        `((term (error (missing_close_paren_for ,(first stack)))) ,@(cddr stack))
    (if (and (operand? (first stack)) (operand? (second stack)))
        `((term (?OP? ,(getContents (second stack)) ,(getContents (first stack)))) ,@(cddr stack))
    (if (operator? (second stack))
     (let* ((op (second stack))
            (type (getAssoc op)))
       (case type
          ((fx fy) 
              `((term (,(getName op) ,(getContents (first stack)))) ,@(cdddr stack)))
          ((xf yf) 
              `((term (,(getName op) ,(getContents (third stack)))) ,@(cdddr stack)))
          ((xfx xfy yfx)
              `((term (,(getName op) ,(getContents (third stack)) ,(getContents (first stack))))
                ,@(cdddr stack)))
          (else `((term (error (in_reducestack ,stack)))))))
     `((term (error "in_reducestack" ,stack)))))))))

  (define (reduceparens stack)
    (cond 
       ((and (>= (length stack) 3) (openparen? (third stack)))
        (if (equal? (getParenType (first stack)) (getParenType (third stack)))
          `((term (,(string-append (getName (third stack)) (getName (first stack)))
                   ,(getContents (second stack))))
               ,@(cdddr stack))
          `((term (error 
                      (,(string-append "parenmismatch " (getName (third stack)) (getName (first stack)))
                   ,(getContents (second stack)))))
              ,@(cdddr stack))))
       ((and (= (length stack) 2) (closeparen? (first stack)))
        `((term (error (missing_open_paren_for ,(second stack))))))
       (else
        `((term (error (in_reduceparens ,stack)))))))



  (define (show msg x) (display (list msg x)) (newline) x)

  ;; this is to handle the overloading needed for unary - and unary +
  (define (overload-test token)
    (if (and (operator? token) (unary-allowed? token))
      `(op ,(getUnaryAlt token))
       token))

             

  (define (parser string)
     (srparser (tokenizer string) ()))

  parser
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; here is a simple test suite
(define (testvalid)
  (define p (makeparser arithexprs))
  (for-each (lambda(x) (display (list x "parses as" (p x))) (newline)(newline))
   (list
        ""
        "1"
        "x"
        "+"
        "1-2"
        "-1"
        "--1"
        "(1)"
        "[1]"
        "{1}"
        "a:=b"
        "a<=b"
        "sin(pi)"
        "f(x,y)"
        "f(x)+g(x)+h(x)"
        "9!=9*8!"
        "(2)"
        "1+2"
        "-1--2"
        "1+2*3"
        "1+(2*3)"
        "(1+2)*3"
        "((((1)+(2))*(3)))"
        "sq(1+[2,3])"
        "(cos(355/113-4*atan(1))-1)"
)))

(define (testerror)
  (define p (makeparser arithexprs))
  (for-each (lambda(x) (display (list (string-append "\"" x "\"") " parses as" (p x))) (newline)(newline))
   (list
        ")"
        "("
        "()"
        "(]"
        "([a)]"
        "/* abc */"
        "1+2)+3"
        "(1+2*(1+3)"
        "1+2*(1+3))"
        "a b c d"
        "* * * *"
        "*2/"
        "(---)"
        "[***]"
   )))
;; this is a convenience definition
(define iaparse (makeparser arithexprs))


