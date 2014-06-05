;; When loaded into a Scheme, this file write a java file that defines the
;; Scheme derived expressions (syntax) and primitive procedures.  It can be
;; bootstrapped with any existing Scheme, not necessarrily Jscheme.
;; * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
;; * subsequently modified by Jscheme project members
;; * licensed under zlib licence (see license.txt)

(define primitive-procedures '(
  (// "========== SECTION 6.1 BOOLEANS ==========")
  (not 1)            "return U.not(x);"
  (boolean? 1)       "return U.toBool(x instanceof Boolean);"
  (// "========== SECTION 6.2 EQUIVALENCE PREDICATES ==========")
  (eqv? 2)           "return U.toBool(U.eqv(x, y));"
  (eq? 2)            "return U.toBool(x == y || U.TRUE.equals(x) && U.TRUE.equals(y) || U.FALSE.equals(x) && U.FALSE.equals(y));"
  (equal? 2)         "return U.toBool(U.equal(x,y));"
  (// "========== SECTION 6.3 LISTS AND PAIRS ==========")
  (pair? 1)          "return U.toBool(U.isPair(x));"
;  (tail 1)           "return U.toList(x).rest;"
  (cons 2)           "return new Pair(x, y);"
  ((car first) 1)    "return U.toList(x).first;"
  ((cdr rest) 1)     "return U.toList(x).rest;"
  ((set-car!) 2)     "return U.toPair(x).first = y;" 
  ((set-cdr!) 2)     "return U.toPair(x).rest = y;"
  ((second) 1)  "return U.toList(x).second();"
  ((third) 1)  "return U.toList(x).third();"
  ((fourth) 1) "return U.toList(x).nth(3);"
  ((fifth) 1) "return U.toList(x).nth(4);"
  ((sixth) 1) "return U.toList(x).nth(5);"
  ((seventh) 1) "return U.toList(x).nth(6);"
  ((eighth) 1) "return U.toList(x).nth(7);"
  ((ninth) 1) "return U.toList(x).nth(8);"
  ((tenth) 1) "return U.toList(x).nth(9);"
;  ((cadr second) 1)  "return U.toList(x).second();"
;  ((caddr third) 1)  "return U.toList(x).third();"
  ((  caar  cadr    cdar   cddr 
     caaar  caadr  cadar caddr  cdaar  cdadr  cddar  cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr) 1)
                     "for (int i = name.length()-2; i >= 1; i--) {
                        x = (name.charAt(i) == 'a') ? U.toList(x).first 
                          : U.toList(x).rest;
                      }
                      return x;"
  (null? 1)          "return U.toBool(x == Pair.EMPTY);"
  (list? 1)          "return U.toBool(U.isList(x));"
  (list 0 n)         "return x;"
  (length 1)         "return U.toNum(U.toList(x).length());"
  (append 0 n)       "return U.append(U.toList(x));"
  (reverse 1)        "return U.toList(x).reverse();"
  (list-tail 2)      "return U.toList(x).listTail(U.toInt(y));"
  (list-ref 2)       "return U.toList(x).nth(U.toInt(y));"
  (memq 2)           "return U.memberAssoc(x, y, true, 1);"
  (memv 2)           "return U.memberAssoc(x, y, true, 2);"
  (member 2)         "return U.memberAssoc(x, y, true, 3);"
  (assq 2)           "return U.memberAssoc(x, y, false, 1);"
  (assv 2)           "return U.memberAssoc(x, y, false, 2);"
  (assoc 2)          "return U.memberAssoc(x, y, false, 3);"
  (// "========== SECTION 6.4 SYMBOLS ==========")
  (symbol? 1)        "return U.toBool(x instanceof Symbol);"
  (symbol->string 1) "return U.toSym(x).toString();"
  (string->symbol 1) "return Symbol.intern(U.toStr(x));"
  (// "========== SECTION 6.5 NUMBERS ==========")
  ((number? complex? real?) 1) "return U.toBool(x instanceof Number);"
  ((rational? integer?) 1) "return U.toBool(x instanceof Integer ||
                                   (x instanceof Double &&
                                    U.toReal(x) == Math.round(U.toReal(x))));"
  (exact? 1)         "return U.toBool(x instanceof Integer);"
  (inexact? 1)       "return U.toBool(x instanceof Double);"
  (= 1 n)            "return U.toBool(U.numCompute(x, U.toList(y), '='));"
  (< 1 n)            "return U.toBool(U.numCompute(x, U.toList(y), '<'));"
  (> 1 n)            "return U.toBool(U.numCompute(x, U.toList(y), '>'));"
  (<= 1 n)           "return U.toBool(U.numCompute(x, U.toList(y), 'L'));"
  (>= 1 n)           "return U.toBool(U.numCompute(x, U.toList(y), 'G'));"
  (zero? 1)          "return U.toBool(U.ZERO.equals(x) || U.toNum(0).equals(x));"
  (positive? 1)      "return U.toBool(U.toReal(x) > 0.0);"
  (negative? 1)      "return U.toBool(U.toReal(x) < 0.0);"
  (odd? 1)           "return U.toBool(Math.abs(U.toInt(x)) % 2 != 0);"
  (even? 1)          "return U.toBool(Math.abs(U.toInt(x)) % 2 == 0);"       
  (max 1 n)          "return U.numCompute(x, U.toList(y), 'X');"
  (min 1 n)          "return U.numCompute(x, U.toList(y), 'N');"
  (+ 0 n)            "return Op.addMulti(U.toList(x));"
  (* 0 n)            "return Op.mulMulti(U.toList(x));"
  (- 1 2)            "return (y==U.MISSING)? Op.sub(U.toNum(0),x):Op.sub(x,y);"
  (/ 2)              "return Op.div(x,y);"
;  (+ 0 n)            "return (nArgs == 0) ? U.toNum(0) 
;                      : U.numCompute(0, U.toList(x), '+');"
;  (* 0 n)            "return (nArgs == 0) ? U.toNum(1) 
;                      : U.numCompute(1, U.toList(x), '*');"
;  (// "Implemented inessential (- z z ...) and (/ z z ...)")
;  (- 1 n)            "return (y == Pair.EMPTY) ? U.numCompute(0, U.list(x), '-') 
;                      : U.numCompute(x, U.toList(y), '-');"
;  (/ 1 n)            "return (y == Pair.EMPTY) ? U.numCompute(1, U.list(x), '/')
;                      : U.numCompute(x, U.toList(y), '/');"
  (abs 1)            "if (x instanceof Integer) return U.toNum(Math.abs(U.toInt(x)));
                      else return U.toNum(Math.abs(U.toReal(x)));"
  (quotient 2)       "return Op.div(x,y);"
  (remainder 2)      "return Op.mod(x,y);"
  (modulo 2)         "return Op.modulo(x,y);"
  (gcd 0 n)           "return (nArgs==0)?U.toNum(0):U.toNum(Math.abs(U.gcd((Pair)x)));"
  (lcm 0 n)           "return (nArgs==0)?U.toNum(1):U.toNum(Math.abs(U.lcm((Pair)x)));"
  (// "inessential numerator, denominator, rationalize not implemented")
  (floor 1)          "return U.toNum(Math.floor(U.toReal(x)));"
  (ceiling 1)        "return U.toNum(Math.ceil(U.toReal(x)));"
  (truncate 1)       "double d = U.toReal(x); 
                      return U.toNum((d < 0) ? Math.ceil(d) : Math.floor(d));" 
  (round 1)          "return U.toNum(Math.round(U.toReal(x)));"
  (exp 1)            "return U.toNum(Math.exp(U.toReal(x)));"
  (log 1)            "return U.toNum(Math.log(U.toReal(x)));"
  (sin 1)            "return U.toNum(Math.sin(U.toReal(x)));"
  (cos 1)            "return U.toNum(Math.cos(U.toReal(x)));"
  (tan 1)            "return U.toNum(Math.tan(U.toReal(x)));"
  (asin 1)           "return U.toNum(Math.asin(U.toReal(x)));"
  (acos 1)           "return U.toNum(Math.acos(U.toReal(x)));"
  (atan 1)           "return U.toNum(Math.atan(U.toReal(x)));"
  (sqrt 1)           "return U.toNum(Math.sqrt(U.toReal(x)));"
  (expt 2)           "return U.toNum(Math.pow(U.toReal(x), U.toReal(y)));"
  (// "inessential complex arithmetic not implemented")
  (exact->inexact 1) "return U.toNum(U.toReal(x));"
  (inexact->exact 1) "return U.toNum(U.toInt(x));"
  (number->string 1 2) "return U.numberToString(x, y);"
  (string->number 1 2) "return U.stringToNumber(x, y);"
                        
  (// "========== SECTION 6.6 CHARACTERS ==========")
  (char? 1)            "return U.toBool(x instanceof Character);"
  (char=? 2)           "return U.toBool(U.to_char(x) == U.to_char(y));"
  (char<? 2)           "return U.toBool(U.to_char(x) <  U.to_char(y));"
  (char>? 2)           "return U.toBool(U.to_char(x) >  U.to_char(y));"
  (char>=? 2)          "return U.toBool(U.to_char(x) >= U.to_char(y));"
  (char<=? 2)          "return U.toBool(U.to_char(x) <= U.to_char(y));"
  (char-ci=? 2)        "return U.toBool(U.to_lc_char(x) == U.to_lc_char(y));"
  (char-ci<? 2)        "return U.toBool(U.to_lc_char(x) <  U.to_lc_char(y));"
  (char-ci>? 2)        "return U.toBool(U.to_lc_char(x) >  U.to_lc_char(y));"
  (char-ci>=? 2)       "return U.toBool(U.to_lc_char(x) >= U.to_lc_char(y));"
  (char-ci<=? 2)       "return U.toBool(U.to_lc_char(x) <= U.to_lc_char(y));"
  (char-alphabetic? 1) "return U.toBool(Character.isLetter(U.to_char(x)));"
  (char-numeric? 1)    "return U.toBool(Character.isDigit(U.to_char(x)));"
  (char-whitespace? 1) "return U.toBool(Character.isWhitespace(U.to_char(x)));"
  (char-upper-case? 1) "return U.toBool(Character.isUpperCase(U.to_char(x)));"
  (char-lower-case? 1) "return U.toBool(Character.isLowerCase(U.to_char(x)));"
  (char->integer 1)    "return U.toNum((int)U.to_char(x));"
  (integer->char 1)    "return U.toChar((char)U.toInt(x));"
  (char-upcase 1)      "return U.toChar(Character.toUpperCase(U.to_char(x)));"
  (char-downcase 1)    "return U.toChar(Character.toLowerCase(U.to_char(x)));"
  (// "========== SECTION 6.7 STRINGS ==========")
  (string? 1)          "return U.toBool(x instanceof String);"
  (make-string 1 2)    "return U.makeString(U.toInt(x), y);"
  (string 0 n)         "return U.listToString(x);"
  (string-length 1)    "return U.toNum(U.toStr(x).length());"
  (string-ref 2)       "return U.toChar(U.toStr(x).charAt(U.toInt(y)));"
;;; KRA 05JAN04: Provide string-set! in primproc.
;  (string-set! 3)      "return E.error(\"string-set! not implemented\", args);"
  (string=? 2)         "return U.toBool(U.toStr(x).equals(y));"
  (string-ci=? 2)      "return U.toBool(U.toStr(x).equalsIgnoreCase(U.toStr(y)));"
  (string<? 2)         "return U.toBool(U.toStr(x).compareTo(U.toStr(y)) < 0);"
  (string>? 2)         "return U.toBool(U.toStr(x).compareTo(U.toStr(y)) > 0);"
  (string>=? 2)        "return U.toBool(U.toStr(x).compareTo(U.toStr(y)) >= 0);"
  (string<=? 2)        "return U.toBool(U.toStr(x).compareTo(U.toStr(y)) <= 0);"
  (string-ci<? 2)      "return U.toBool(U.stringCompareIgnoreCase(x, y) < 0);"
  (string-ci>? 2)      "return U.toBool(U.stringCompareIgnoreCase(x, y) > 0);"
  (string-ci>=? 2)     "return U.toBool(U.stringCompareIgnoreCase(x, y) >= 0);"
  (string-ci<=? 2)     "return U.toBool(U.stringCompareIgnoreCase(x, y) <= 0);"
  (substring 3)        "int start = U.toInt(y);
                        return U.toStr(x).substring(start, U.toInt(z));"
  (string-append 0 n)  "return U.stringAppend(U.toList(x));"
  (string->list 1)     "return U.stringToList(x);"
  (list->string 1)     "return U.listToString(x);"
  (// "Inessential string-copy and string-fill! implemented in primprocs.scm")
  (// "========== SECTION 6.8 VECTORS ==========")
  ;; KRA 01OCT01: New vector semantics
  (vector? 1)          "return U.toBool(x instanceof Object[] || x != null && x.getClass().isArray());"
  (vector-fill! 2)     "return U.vectorFill(x,y);"
;;  (make-vector 1)       "return U.makeVector(x);"
  (make-vector 1 2)     "return (y==U.MISSING)?U.makeVector(x):U.makeVector(x,y);"
  (vector 0 n)          "return U.listToVector(x);"
  (vector-length 1)     "return U.vectorLength(x);"
  (vector-ref 2)        "return U.vectorRef(x, y);"
  (vector-set! 3)       "return U.vectorSet(x, y, z);"
  (vector->list 1)      "return U.vectorToList(U.toVec(x));"
  (list->vector 1)      "return U.listToVector(x);"
  (// "========== SECTION 6.9 CONTROL FEATURES ==========")
  (procedure? 1)       "return U.toBool(x instanceof Procedure);"
  ;; KRA 18JUN02: (apply proc ... arg) as in R5RS and Common Lisp.
  ;; (apply 2)            "Procedure p = U.toProc(x); 
  ;;                       return p.apply(U.toList(y));"
  (apply 1 n)         "return U.apply(U.toProc(x), U.toList(y));"
  (map 2 n)            "return U.map(U.toProc(x), new Pair(y, z), U.list(U.TRUE));"
  (for-each 2 n)       "return U.map(U.toProc(x), new Pair(y, z), Pair.EMPTY);"
  (force 1)            "return (!(x instanceof Procedure)) ? x
                        : U.toProc(x).apply(U.NO_ARGS);"
  ((call/cc call-with-current-continuation) 1) "return U.callCC(U.toProc(x));"
  (eval 1 2)             "return Scheme.eval(x, y);"
  (null-environment 0)     "return Scheme.getNullEnvironment();"
  (interaction-environment 0)     "return Scheme.getInteractionEnvironment();"
  (// "========== SECTION 6.10 INPUT AND OUPUT ==========")
  (call-with-input-file 2) "return U.callWithInputFile(x, U.toProc(y));"
  (call-with-output-file 2)"return U.callWithOutputFile(x, U.toProc(y));"
  (input-port? 1)          "return U.toBool(x instanceof InputPort);"
  (output-port? 1)         "return U.toBool(x instanceof PrintWriter);"
  (current-input-port 0)   "return U.toInPort(U.MISSING);"
  (current-output-port 0)  "return U.toOutPort(U.MISSING);"
  (// "Inessential with-input-from-file, with-output-to-file not implemented")
  (open-input-file 1)      "return U.openInputFile(x);"
  (open-output-file 1)     "return U.openOutputFile(x);"
  (close-input-port 1)     "return U.toInPort(x).close(); "
  (close-output-port 1)    "U.toOutPort(x).close(); return U.TRUE;"
  (read 0 1)               "return U.toInPort(x).read(); "
  (read-char 0 1)          "return U.toInPort(x).readChar();"
  (peek-char 0 1)          "return U.toInPort(x).peekChar();"
  (eof-object? 1)          "return U.toBool(x == InputPort.EOF);"
  (// "Inessential char-ready?, transcript-on, transcript-off not implemented")
  (write 1 2)              "return U.write(x, U.toOutPort(y), true);"
  (display 1 2)            "U.write(x, U.toOutPort(y), false); return DO_NOT_DISPLAY;"
  (newline 0 1)            "U.toOutPort(x).println();
                            U.toOutPort(x).flush(); return U.TRUE;"
  (write-char 1 2)         "U.toOutPort(y).print(U.to_char(x)); return U.TRUE;"
  (load 1)                 "return Scheme.load(x);"
  (// "========== EXTENSIONS ==========")
  (set-procedure-name! 2) "((Procedure)x).setName(y); return x;"
  (macroexpand 1)          "return Macro.expand(U.toPair(x));"
  (error 0 n)              "return E.error(\"\", x);"
  (class 1)                "return U.maybeToClass(x);"
  (import 1)               "Import.addImport(U.toStr(x)); return U.TRUE;"
  (constructor 1 n)        "return new RawConstructor(Invoke.findConstructor(x, U.toList(y)));"
  (method 2 n)             "return new RawMethod(Invoke.findMethod(U.toStr(x),y , ((Pair) z)));"
  (new 1 n)                "return Invoke.invokeConstructor(U.toClass(x).getName(), U.listToVector(y));"
  (invoke 2 n)             "return Invoke.invokeInstance(x, y.toString(), U.listToVector(z), false);"
  (invoke-static 2 n)      "return Invoke.invokeStatic(U.toClass(x),y.toString(), U.listToVector(z));"
  (peek 2)                 "return Invoke.peek(x, U.toStr(y));"
  (peek-static 2)          "return Invoke.peekStatic(U.toClass(x), U.toStr(y));"
  (poke 3)                 "return Invoke.poke(x, U.toStr(y), z);"
  (poke-static 3)          "return Invoke.pokeStatic(U.toClass(x), U.toStr(y), z);"
  (exit 0 0)               "return U.toBool(Scheme.currentEvaluator().setExit(true));"
  (time-call 2)            "return U.timeCall(U.toProc(x), U.toInt(y, 1));"
  (list->array 2)          "return U.listToArray(U.toClass(x),U.toList(y));"
  (array->list 1)          "return U.arrayToList(x);"
  (% 2)                    "return Op.mod(x, y);"
  (& 2)                    "return Op.and(x, y);"
  (| 2)                    "return Op.or (x, y);"
  (^ 2)                    "return Op.xor(x, y);"
  (~ 1)                    "return Op.complement(x);"
  (!= 2)                   "return U.toBool(Op.ne(x,y));"
  (<< 2)                   "return Op.leftShift(x, y);"
  (>> 2)                   "return Op.rightShift(x, y);"
  (>>> 2)                  "return Op.rightShiftZ(x, y);"
  (throw 1)                "return Procedure.throwRuntimeException(new JschemeThrowable(x));"
;  (tryCatch 2)             "return Procedure.tryCatch(x, y);"
  (synchronize 2)          "return Procedure.synchronize(x, U.toProc(y));"
  (string->expr 1)         "return (x==null)?null:((Pair)jscheme.REPL.parseScheme((String)x)).first;"
  (string->exprlist 1)     "return (x==null)?null:jscheme.REPL.parseScheme((String) x);"
  (initial-environment 0)  "return Scheme.getInitialEnvironment();"
  (load-environment 1)  "return Scheme.loadEnvironment(x);"
  (environment-bindings 1) "return DynamicEnvironment.getBindings(x);"
  (environment-import 1 2)   "return Scheme.environmentImport(x, y);"
  (language-import 1)      "return Scheme.languageImport(x);"
  (values 0 n)             "return Values.values(U.toList(x));"
  (call-with-values 2)     "return Values.callWithValues(U.toProc(x), U.toProc(y));"
  ;; KRA 23NOV04: 
  (isNull 1)               "return U.toBool(x == null);"
  (!isNull 1)              "return U.toBool(x != null);"

)) ;; end primitive-procedures
  
(define (genPrimitives)

  (define out (java.io.PrintWriter. (java.io.PrintStream. (java.io.FileOutputStream. (java.io.File. "src/jsint" "Primitive.java")))))

  (define installs '());; List of primitives to install
  (define opcode 0);; Current opcode
  (define (print   . args) (map (lambda(x) (display x out)) args))
  (define (println . args) (map (lambda(x) (display x out)) args) (newline out))
  (define (mklist x) (if (pair? x) x (list x)))

  (define (derived-expressions)
    (call-with-input-file "src/jsint/primproc.scm"
      (let* ((string (java.io.StringWriter.))
	     (o (java.io.PrintWriter. string)))
	(lambda (s) (write (read s) o)
		(write (.toString string) out)))))

  (define (process-primitives prims)
    (if (not (eq? prims '()))
	(if (eq? (first (first prims)) '//)
	    (begin (println "    // " (second (first prims)))
		   (process-primitives (rest prims)))
	    (begin (process-entry (first prims) (second prims))
		   (process-primitives (rest (rest prims)))))))
;; spec is of the form (name min {max})
;; where name is either a symbol or list of symbols
;; where min is an integer, and max is an integer, or n, or is absent
  (define (process-entry spec code)
    (let ((names (mklist (first spec)))
          (min (second spec))
          (max (if (= (length spec) 3) (third spec) (second spec))))
      (set! opcode (+ opcode 1))
      (print "    case " opcode " /* ")
      (for-each (lambda (name)
                  (set! installs (cons (list "    new Primitive(\"" name "\", " 
                                             opcode ", " min ", " max ");
")
                                       installs))
                  (print  name " "))
		names)
      (println "*/: " code)))
  (define (process-file)
    (set! opcode 0)
    (set! installs '())
    (println "package jsint;
  
import java.io.*;
import java.lang.reflect.Array;
/** Primitive procedures (as defined in the R4RS Scheme report.
 * NOTE: Primitive.java IS GENERATED FROM primitives.scm. EDIT AT YOUR OWN RISK.
 * **/
public class Primitive extends Procedure {
  int opcode;
	   
  /** Scheme Droid Modification
   * A singleton-like object to return to tell SchemeDroid not to print the
   * return value.
   */
  public static Object DO_NOT_DISPLAY = new Object();
  /* end Scheme Droid modification */

  /** Constructor **/
  public Primitive(String name, int opcode, int minArgs, int maxArgs) {
    this.name = name; 
    this.opcode = opcode;
    this.minArgs = minArgs; 
    this.maxArgs = maxArgs;
    // Either fixed number of parms or 1 optional param or a \"rest\" parameter
    if (!(maxArgs == minArgs || maxArgs == minArgs+1 || maxArgs == Integer.MAX_VALUE))
      E.warn(\"Bad value of maxArgs: \" + maxArgs, name);
    Symbol.intern(name).setGlobalValue(this);
  }
  
  /** Apply the primitive to a list of arguments. **/
  public Object apply(Object[] args) {
    int nArgs = args.length;
    Object x = (nArgs >= 1) ? args[0] : U.MISSING;
    Object y = (nArgs >= 2) ? args[1] : U.MISSING;
    Object z = (nArgs >= 3) ? args[2] : U.MISSING;
        
    switch(opcode) {")
    (process-primitives primitive-procedures)
    (print "    }
    return E.error(\"internal error: unknown primitive opcode\" +
                                opcode + \" applied to \" + args);
  }
  /* Create the primitive procedures */
    static void loadPrimitives() {
    int n = Integer.MAX_VALUE;
")
    (map (lambda (x) (apply print x)) installs)
    (print "    String derived = ")
    (derived-expressions)
    (println ";")
    (println "    //Scheme.load(\"jsint/primproc.scm\");
    Scheme.load(new InputPort(new java.io.StringReader(derived)));
    primitives_loaded=true;
    }

    static boolean primitives_loaded = false;}"))
  (process-file)
  (.close out)
  )					; End genPrimitives
(genPrimitives)
