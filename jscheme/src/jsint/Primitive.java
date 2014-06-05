package jsint;
  
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
    // Either fixed number of parms or 1 optional param or a "rest" parameter
    if (!(maxArgs == minArgs || maxArgs == minArgs+1 || maxArgs == Integer.MAX_VALUE))
      E.warn("Bad value of maxArgs: " + maxArgs, name);
    Symbol.intern(name).setGlobalValue(this);
  }
  
  /** Apply the primitive to a list of arguments. **/
  public Object apply(Object[] args) {
    int nArgs = args.length;
    Object x = (nArgs >= 1) ? args[0] : U.MISSING;
    Object y = (nArgs >= 2) ? args[1] : U.MISSING;
    Object z = (nArgs >= 3) ? args[2] : U.MISSING;
        
    switch(opcode) {
    // ========== SECTION 6.1 BOOLEANS ==========
    case 1 /* not */: return U.not(x);
    case 2 /* boolean? */: return U.toBool(x instanceof Boolean);
    // ========== SECTION 6.2 EQUIVALENCE PREDICATES ==========
    case 3 /* eqv? */: return U.toBool(U.eqv(x, y));
    case 4 /* eq? */: return U.toBool(x == y || U.TRUE.equals(x) && U.TRUE.equals(y) || U.FALSE.equals(x) && U.FALSE.equals(y));
    case 5 /* equal? */: return U.toBool(U.equal(x,y));
    // ========== SECTION 6.3 LISTS AND PAIRS ==========
    case 6 /* pair? */: return U.toBool(U.isPair(x));
    case 7 /* cons */: return new Pair(x, y);
    case 8 /* car first */: return U.toList(x).first;
    case 9 /* cdr rest */: return U.toList(x).rest;
    case 10 /* set-car! */: return U.toPair(x).first = y;
    case 11 /* set-cdr! */: return U.toPair(x).rest = y;
    case 12 /* second */: return U.toList(x).second();
    case 13 /* third */: return U.toList(x).third();
    case 14 /* fourth */: return U.toList(x).nth(3);
    case 15 /* fifth */: return U.toList(x).nth(4);
    case 16 /* sixth */: return U.toList(x).nth(5);
    case 17 /* seventh */: return U.toList(x).nth(6);
    case 18 /* eighth */: return U.toList(x).nth(7);
    case 19 /* ninth */: return U.toList(x).nth(8);
    case 20 /* tenth */: return U.toList(x).nth(9);
    case 21 /* caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr */: for (int i = name.length()-2; i >= 1; i--) {
                        x = (name.charAt(i) == 'a') ? U.toList(x).first 
                          : U.toList(x).rest;
                      }
                      return x;
    case 22 /* null? */: return U.toBool(x == Pair.EMPTY);
    case 23 /* list? */: return U.toBool(U.isList(x));
    case 24 /* list */: return x;
    case 25 /* length */: return U.toNum(U.toList(x).length());
    case 26 /* append */: return U.append(U.toList(x));
    case 27 /* reverse */: return U.toList(x).reverse();
    case 28 /* list-tail */: return U.toList(x).listTail(U.toInt(y));
    case 29 /* list-ref */: return U.toList(x).nth(U.toInt(y));
    case 30 /* memq */: return U.memberAssoc(x, y, true, 1);
    case 31 /* memv */: return U.memberAssoc(x, y, true, 2);
    case 32 /* member */: return U.memberAssoc(x, y, true, 3);
    case 33 /* assq */: return U.memberAssoc(x, y, false, 1);
    case 34 /* assv */: return U.memberAssoc(x, y, false, 2);
    case 35 /* assoc */: return U.memberAssoc(x, y, false, 3);
    // ========== SECTION 6.4 SYMBOLS ==========
    case 36 /* symbol? */: return U.toBool(x instanceof Symbol);
    case 37 /* symbol->string */: return U.toSym(x).toString();
    case 38 /* string->symbol */: return Symbol.intern(U.toStr(x));
    // ========== SECTION 6.5 NUMBERS ==========
    case 39 /* number? complex? real? */: return U.toBool(x instanceof Number);
    case 40 /* rational? integer? */: return U.toBool(x instanceof Integer ||
                                   (x instanceof Double &&
                                    U.toReal(x) == Math.round(U.toReal(x))));
    case 41 /* exact? */: return U.toBool(x instanceof Integer);
    case 42 /* inexact? */: return U.toBool(x instanceof Double);
    case 43 /* = */: return U.toBool(U.numCompute(x, U.toList(y), '='));
    case 44 /* < */: return U.toBool(U.numCompute(x, U.toList(y), '<'));
    case 45 /* > */: return U.toBool(U.numCompute(x, U.toList(y), '>'));
    case 46 /* <= */: return U.toBool(U.numCompute(x, U.toList(y), 'L'));
    case 47 /* >= */: return U.toBool(U.numCompute(x, U.toList(y), 'G'));
    case 48 /* zero? */: return U.toBool(U.ZERO.equals(x) || U.toNum(0).equals(x));
    case 49 /* positive? */: return U.toBool(U.toReal(x) > 0.0);
    case 50 /* negative? */: return U.toBool(U.toReal(x) < 0.0);
    case 51 /* odd? */: return U.toBool(Math.abs(U.toInt(x)) % 2 != 0);
    case 52 /* even? */: return U.toBool(Math.abs(U.toInt(x)) % 2 == 0);
    case 53 /* max */: return U.numCompute(x, U.toList(y), 'X');
    case 54 /* min */: return U.numCompute(x, U.toList(y), 'N');
    case 55 /* + */: return Op.addMulti(U.toList(x));
    case 56 /* * */: return Op.mulMulti(U.toList(x));
    case 57 /* - */: return (y==U.MISSING)? Op.sub(U.toNum(0),x):Op.sub(x,y);
    case 58 /* / */: return Op.div(x,y);
    case 59 /* abs */: if (x instanceof Integer) return U.toNum(Math.abs(U.toInt(x)));
                      else return U.toNum(Math.abs(U.toReal(x)));
    case 60 /* quotient */: return Op.div(x,y);
    case 61 /* remainder */: return Op.mod(x,y);
    case 62 /* modulo */: return Op.modulo(x,y);
    case 63 /* gcd */: return (nArgs==0)?U.toNum(0):U.toNum(Math.abs(U.gcd((Pair)x)));
    case 64 /* lcm */: return (nArgs==0)?U.toNum(1):U.toNum(Math.abs(U.lcm((Pair)x)));
    // inessential numerator, denominator, rationalize not implemented
    case 65 /* floor */: return U.toNum(Math.floor(U.toReal(x)));
    case 66 /* ceiling */: return U.toNum(Math.ceil(U.toReal(x)));
    case 67 /* truncate */: double d = U.toReal(x); 
                      return U.toNum((d < 0) ? Math.ceil(d) : Math.floor(d));
    case 68 /* round */: return U.toNum(Math.round(U.toReal(x)));
    case 69 /* exp */: return U.toNum(Math.exp(U.toReal(x)));
    case 70 /* log */: return U.toNum(Math.log(U.toReal(x)));
    case 71 /* sin */: return U.toNum(Math.sin(U.toReal(x)));
    case 72 /* cos */: return U.toNum(Math.cos(U.toReal(x)));
    case 73 /* tan */: return U.toNum(Math.tan(U.toReal(x)));
    case 74 /* asin */: return U.toNum(Math.asin(U.toReal(x)));
    case 75 /* acos */: return U.toNum(Math.acos(U.toReal(x)));
    case 76 /* atan */: return U.toNum(Math.atan(U.toReal(x)));
    case 77 /* sqrt */: return U.toNum(Math.sqrt(U.toReal(x)));
    case 78 /* expt */: return U.toNum(Math.pow(U.toReal(x), U.toReal(y)));
    // inessential complex arithmetic not implemented
    case 79 /* exact->inexact */: return U.toNum(U.toReal(x));
    case 80 /* inexact->exact */: return U.toNum(U.toInt(x));
    case 81 /* number->string */: return U.numberToString(x, y);
    case 82 /* string->number */: return U.stringToNumber(x, y);
    // ========== SECTION 6.6 CHARACTERS ==========
    case 83 /* char? */: return U.toBool(x instanceof Character);
    case 84 /* char=? */: return U.toBool(U.to_char(x) == U.to_char(y));
    case 85 /* char<? */: return U.toBool(U.to_char(x) <  U.to_char(y));
    case 86 /* char>? */: return U.toBool(U.to_char(x) >  U.to_char(y));
    case 87 /* char>=? */: return U.toBool(U.to_char(x) >= U.to_char(y));
    case 88 /* char<=? */: return U.toBool(U.to_char(x) <= U.to_char(y));
    case 89 /* char-ci=? */: return U.toBool(U.to_lc_char(x) == U.to_lc_char(y));
    case 90 /* char-ci<? */: return U.toBool(U.to_lc_char(x) <  U.to_lc_char(y));
    case 91 /* char-ci>? */: return U.toBool(U.to_lc_char(x) >  U.to_lc_char(y));
    case 92 /* char-ci>=? */: return U.toBool(U.to_lc_char(x) >= U.to_lc_char(y));
    case 93 /* char-ci<=? */: return U.toBool(U.to_lc_char(x) <= U.to_lc_char(y));
    case 94 /* char-alphabetic? */: return U.toBool(Character.isLetter(U.to_char(x)));
    case 95 /* char-numeric? */: return U.toBool(Character.isDigit(U.to_char(x)));
    case 96 /* char-whitespace? */: return U.toBool(Character.isWhitespace(U.to_char(x)));
    case 97 /* char-upper-case? */: return U.toBool(Character.isUpperCase(U.to_char(x)));
    case 98 /* char-lower-case? */: return U.toBool(Character.isLowerCase(U.to_char(x)));
    case 99 /* char->integer */: return U.toNum((int)U.to_char(x));
    case 100 /* integer->char */: return U.toChar((char)U.toInt(x));
    case 101 /* char-upcase */: return U.toChar(Character.toUpperCase(U.to_char(x)));
    case 102 /* char-downcase */: return U.toChar(Character.toLowerCase(U.to_char(x)));
    // ========== SECTION 6.7 STRINGS ==========
    case 103 /* string? */: return U.toBool(x instanceof String);
    case 104 /* make-string */: return U.makeString(U.toInt(x), y);
    case 105 /* string */: return U.listToString(x);
    case 106 /* string-length */: return U.toNum(U.toStr(x).length());
    case 107 /* string-ref */: return U.toChar(U.toStr(x).charAt(U.toInt(y)));
    case 108 /* string=? */: return U.toBool(U.toStr(x).equals(y));
    case 109 /* string-ci=? */: return U.toBool(U.toStr(x).equalsIgnoreCase(U.toStr(y)));
    case 110 /* string<? */: return U.toBool(U.toStr(x).compareTo(U.toStr(y)) < 0);
    case 111 /* string>? */: return U.toBool(U.toStr(x).compareTo(U.toStr(y)) > 0);
    case 112 /* string>=? */: return U.toBool(U.toStr(x).compareTo(U.toStr(y)) >= 0);
    case 113 /* string<=? */: return U.toBool(U.toStr(x).compareTo(U.toStr(y)) <= 0);
    case 114 /* string-ci<? */: return U.toBool(U.stringCompareIgnoreCase(x, y) < 0);
    case 115 /* string-ci>? */: return U.toBool(U.stringCompareIgnoreCase(x, y) > 0);
    case 116 /* string-ci>=? */: return U.toBool(U.stringCompareIgnoreCase(x, y) >= 0);
    case 117 /* string-ci<=? */: return U.toBool(U.stringCompareIgnoreCase(x, y) <= 0);
    case 118 /* substring */: int start = U.toInt(y);
                        return U.toStr(x).substring(start, U.toInt(z));
    case 119 /* string-append */: return U.stringAppend(U.toList(x));
    case 120 /* string->list */: return U.stringToList(x);
    case 121 /* list->string */: return U.listToString(x);
    // Inessential string-copy and string-fill! implemented in primprocs.scm
    // ========== SECTION 6.8 VECTORS ==========
    case 122 /* vector? */: return U.toBool(x instanceof Object[] || x != null && x.getClass().isArray());
    case 123 /* vector-fill! */: return U.vectorFill(x,y);
    case 124 /* make-vector */: return (y==U.MISSING)?U.makeVector(x):U.makeVector(x,y);
    case 125 /* vector */: return U.listToVector(x);
    case 126 /* vector-length */: return U.vectorLength(x);
    case 127 /* vector-ref */: return U.vectorRef(x, y);
    case 128 /* vector-set! */: return U.vectorSet(x, y, z);
    case 129 /* vector->list */: return U.vectorToList(U.toVec(x));
    case 130 /* list->vector */: return U.listToVector(x);
    // ========== SECTION 6.9 CONTROL FEATURES ==========
    case 131 /* procedure? */: return U.toBool(x instanceof Procedure);
    case 132 /* apply */: return U.apply(U.toProc(x), U.toList(y));
    case 133 /* map */: return U.map(U.toProc(x), new Pair(y, z), U.list(U.TRUE));
    case 134 /* for-each */: return U.map(U.toProc(x), new Pair(y, z), Pair.EMPTY);
    case 135 /* force */: return (!(x instanceof Procedure)) ? x
                        : U.toProc(x).apply(U.NO_ARGS);
    case 136 /* call/cc call-with-current-continuation */: return U.callCC(U.toProc(x));
    case 137 /* eval */: return Scheme.eval(x, y);
    case 138 /* null-environment */: return Scheme.getNullEnvironment();
    case 139 /* interaction-environment */: return Scheme.getInteractionEnvironment();
    // ========== SECTION 6.10 INPUT AND OUPUT ==========
    case 140 /* call-with-input-file */: return U.callWithInputFile(x, U.toProc(y));
    case 141 /* call-with-output-file */: return U.callWithOutputFile(x, U.toProc(y));
    case 142 /* input-port? */: return U.toBool(x instanceof InputPort);
    case 143 /* output-port? */: return U.toBool(x instanceof PrintWriter);
    case 144 /* current-input-port */: return U.toInPort(U.MISSING);
    case 145 /* current-output-port */: return U.toOutPort(U.MISSING);
    // Inessential with-input-from-file, with-output-to-file not implemented
    case 146 /* open-input-file */: return U.openInputFile(x);
    case 147 /* open-output-file */: return U.openOutputFile(x);
    case 148 /* close-input-port */: return U.toInPort(x).close(); 
    case 149 /* close-output-port */: U.toOutPort(x).close(); return U.TRUE;
    case 150 /* read */: return U.toInPort(x).read(); 
    case 151 /* read-char */: return U.toInPort(x).readChar();
    case 152 /* peek-char */: return U.toInPort(x).peekChar();
    case 153 /* eof-object? */: return U.toBool(x == InputPort.EOF);
    // Inessential char-ready?, transcript-on, transcript-off not implemented
    case 154 /* write */: return U.write(x, U.toOutPort(y), true);
    case 155 /* display */: U.write(x, U.toOutPort(y), false); return DO_NOT_DISPLAY;
    case 156 /* newline */: U.toOutPort(x).println();
                            U.toOutPort(x).flush(); return U.TRUE;
    case 157 /* write-char */: U.toOutPort(y).print(U.to_char(x)); return U.TRUE;
    case 158 /* load */: return Scheme.load(x);
    // ========== EXTENSIONS ==========
    case 159 /* set-procedure-name! */: ((Procedure)x).setName(y); return x;
    case 160 /* macroexpand */: return Macro.expand(U.toPair(x));
    case 161 /* error */: return E.error("", x);
    case 162 /* class */: return U.maybeToClass(x);
    case 163 /* import */: Import.addImport(U.toStr(x)); return U.TRUE;
    case 164 /* constructor */: return new RawConstructor(Invoke.findConstructor(x, U.toList(y)));
    case 165 /* method */: return new RawMethod(Invoke.findMethod(U.toStr(x),y , ((Pair) z)));
    case 166 /* new */: return Invoke.invokeConstructor(U.toClass(x).getName(), U.listToVector(y));
    case 167 /* invoke */: return Invoke.invokeInstance(x, y.toString(), U.listToVector(z), false);
    case 168 /* invoke-static */: return Invoke.invokeStatic(U.toClass(x),y.toString(), U.listToVector(z));
    case 169 /* peek */: return Invoke.peek(x, U.toStr(y));
    case 170 /* peek-static */: return Invoke.peekStatic(U.toClass(x), U.toStr(y));
    case 171 /* poke */: return Invoke.poke(x, U.toStr(y), z);
    case 172 /* poke-static */: return Invoke.pokeStatic(U.toClass(x), U.toStr(y), z);
    case 173 /* exit */: return U.toBool(Scheme.currentEvaluator().setExit(true));
    case 174 /* time-call */: return U.timeCall(U.toProc(x), U.toInt(y, 1));
    case 175 /* list->array */: return U.listToArray(U.toClass(x),U.toList(y));
    case 176 /* array->list */: return U.arrayToList(x);
    case 177 /* % */: return Op.mod(x, y);
    case 178 /* & */: return Op.and(x, y);
    case 179 /* | */: return Op.or (x, y);
    case 180 /* ^ */: return Op.xor(x, y);
    case 181 /* ~ */: return Op.complement(x);
    case 182 /* != */: return U.toBool(Op.ne(x,y));
    case 183 /* << */: return Op.leftShift(x, y);
    case 184 /* >> */: return Op.rightShift(x, y);
    case 185 /* >>> */: return Op.rightShiftZ(x, y);
    case 186 /* throw */: return Procedure.throwRuntimeException(new JschemeThrowable(x));
    case 187 /* synchronize */: return Procedure.synchronize(x, U.toProc(y));
    case 188 /* string->expr */: return (x==null)?null:((Pair)jscheme.REPL.parseScheme((String)x)).first;
    case 189 /* string->exprlist */: return (x==null)?null:jscheme.REPL.parseScheme((String) x);
    case 190 /* initial-environment */: return Scheme.getInitialEnvironment();
    case 191 /* load-environment */: return Scheme.loadEnvironment(x);
    case 192 /* environment-bindings */: return DynamicEnvironment.getBindings(x);
    case 193 /* environment-import */: return Scheme.environmentImport(x, y);
    case 194 /* language-import */: return Scheme.languageImport(x);
    case 195 /* values */: return Values.values(U.toList(x));
    case 196 /* call-with-values */: return Values.callWithValues(U.toProc(x), U.toProc(y));
    case 197 /* isNull */: return U.toBool(x == null);
    case 198 /* !isNull */: return U.toBool(x != null);
    }
    return E.error("internal error: unknown primitive opcode" +
                                opcode + " applied to " + args);
  }
  /* Create the primitive procedures */
    static void loadPrimitives() {
    int n = Integer.MAX_VALUE;
    new Primitive("!isNull", 198, 1, 1);
    new Primitive("isNull", 197, 1, 1);
    new Primitive("call-with-values", 196, 2, 2);
    new Primitive("values", 195, 0, n);
    new Primitive("language-import", 194, 1, 1);
    new Primitive("environment-import", 193, 1, 2);
    new Primitive("environment-bindings", 192, 1, 1);
    new Primitive("load-environment", 191, 1, 1);
    new Primitive("initial-environment", 190, 0, 0);
    new Primitive("string->exprlist", 189, 1, 1);
    new Primitive("string->expr", 188, 1, 1);
    new Primitive("synchronize", 187, 2, 2);
    new Primitive("throw", 186, 1, 1);
    new Primitive(">>>", 185, 2, 2);
    new Primitive(">>", 184, 2, 2);
    new Primitive("<<", 183, 2, 2);
    new Primitive("!=", 182, 2, 2);
    new Primitive("~", 181, 1, 1);
    new Primitive("^", 180, 2, 2);
    new Primitive("|", 179, 2, 2);
    new Primitive("&", 178, 2, 2);
    new Primitive("%", 177, 2, 2);
    new Primitive("array->list", 176, 1, 1);
    new Primitive("list->array", 175, 2, 2);
    new Primitive("time-call", 174, 2, 2);
    new Primitive("exit", 173, 0, 0);
    new Primitive("poke-static", 172, 3, 3);
    new Primitive("poke", 171, 3, 3);
    new Primitive("peek-static", 170, 2, 2);
    new Primitive("peek", 169, 2, 2);
    new Primitive("invoke-static", 168, 2, n);
    new Primitive("invoke", 167, 2, n);
    new Primitive("new", 166, 1, n);
    new Primitive("method", 165, 2, n);
    new Primitive("constructor", 164, 1, n);
    new Primitive("import", 163, 1, 1);
    new Primitive("class", 162, 1, 1);
    new Primitive("error", 161, 0, n);
    new Primitive("macroexpand", 160, 1, 1);
    new Primitive("set-procedure-name!", 159, 2, 2);
    new Primitive("load", 158, 1, 1);
    new Primitive("write-char", 157, 1, 2);
    new Primitive("newline", 156, 0, 1);
    new Primitive("display", 155, 1, 2);
    new Primitive("write", 154, 1, 2);
    new Primitive("eof-object?", 153, 1, 1);
    new Primitive("peek-char", 152, 0, 1);
    new Primitive("read-char", 151, 0, 1);
    new Primitive("read", 150, 0, 1);
    new Primitive("close-output-port", 149, 1, 1);
    new Primitive("close-input-port", 148, 1, 1);
    new Primitive("open-output-file", 147, 1, 1);
    new Primitive("open-input-file", 146, 1, 1);
    new Primitive("current-output-port", 145, 0, 0);
    new Primitive("current-input-port", 144, 0, 0);
    new Primitive("output-port?", 143, 1, 1);
    new Primitive("input-port?", 142, 1, 1);
    new Primitive("call-with-output-file", 141, 2, 2);
    new Primitive("call-with-input-file", 140, 2, 2);
    new Primitive("interaction-environment", 139, 0, 0);
    new Primitive("null-environment", 138, 0, 0);
    new Primitive("eval", 137, 1, 2);
    new Primitive("call-with-current-continuation", 136, 1, 1);
    new Primitive("call/cc", 136, 1, 1);
    new Primitive("force", 135, 1, 1);
    new Primitive("for-each", 134, 2, n);
    new Primitive("map", 133, 2, n);
    new Primitive("apply", 132, 1, n);
    new Primitive("procedure?", 131, 1, 1);
    new Primitive("list->vector", 130, 1, 1);
    new Primitive("vector->list", 129, 1, 1);
    new Primitive("vector-set!", 128, 3, 3);
    new Primitive("vector-ref", 127, 2, 2);
    new Primitive("vector-length", 126, 1, 1);
    new Primitive("vector", 125, 0, n);
    new Primitive("make-vector", 124, 1, 2);
    new Primitive("vector-fill!", 123, 2, 2);
    new Primitive("vector?", 122, 1, 1);
    new Primitive("list->string", 121, 1, 1);
    new Primitive("string->list", 120, 1, 1);
    new Primitive("string-append", 119, 0, n);
    new Primitive("substring", 118, 3, 3);
    new Primitive("string-ci<=?", 117, 2, 2);
    new Primitive("string-ci>=?", 116, 2, 2);
    new Primitive("string-ci>?", 115, 2, 2);
    new Primitive("string-ci<?", 114, 2, 2);
    new Primitive("string<=?", 113, 2, 2);
    new Primitive("string>=?", 112, 2, 2);
    new Primitive("string>?", 111, 2, 2);
    new Primitive("string<?", 110, 2, 2);
    new Primitive("string-ci=?", 109, 2, 2);
    new Primitive("string=?", 108, 2, 2);
    new Primitive("string-ref", 107, 2, 2);
    new Primitive("string-length", 106, 1, 1);
    new Primitive("string", 105, 0, n);
    new Primitive("make-string", 104, 1, 2);
    new Primitive("string?", 103, 1, 1);
    new Primitive("char-downcase", 102, 1, 1);
    new Primitive("char-upcase", 101, 1, 1);
    new Primitive("integer->char", 100, 1, 1);
    new Primitive("char->integer", 99, 1, 1);
    new Primitive("char-lower-case?", 98, 1, 1);
    new Primitive("char-upper-case?", 97, 1, 1);
    new Primitive("char-whitespace?", 96, 1, 1);
    new Primitive("char-numeric?", 95, 1, 1);
    new Primitive("char-alphabetic?", 94, 1, 1);
    new Primitive("char-ci<=?", 93, 2, 2);
    new Primitive("char-ci>=?", 92, 2, 2);
    new Primitive("char-ci>?", 91, 2, 2);
    new Primitive("char-ci<?", 90, 2, 2);
    new Primitive("char-ci=?", 89, 2, 2);
    new Primitive("char<=?", 88, 2, 2);
    new Primitive("char>=?", 87, 2, 2);
    new Primitive("char>?", 86, 2, 2);
    new Primitive("char<?", 85, 2, 2);
    new Primitive("char=?", 84, 2, 2);
    new Primitive("char?", 83, 1, 1);
    new Primitive("string->number", 82, 1, 2);
    new Primitive("number->string", 81, 1, 2);
    new Primitive("inexact->exact", 80, 1, 1);
    new Primitive("exact->inexact", 79, 1, 1);
    new Primitive("expt", 78, 2, 2);
    new Primitive("sqrt", 77, 1, 1);
    new Primitive("atan", 76, 1, 1);
    new Primitive("acos", 75, 1, 1);
    new Primitive("asin", 74, 1, 1);
    new Primitive("tan", 73, 1, 1);
    new Primitive("cos", 72, 1, 1);
    new Primitive("sin", 71, 1, 1);
    new Primitive("log", 70, 1, 1);
    new Primitive("exp", 69, 1, 1);
    new Primitive("round", 68, 1, 1);
    new Primitive("truncate", 67, 1, 1);
    new Primitive("ceiling", 66, 1, 1);
    new Primitive("floor", 65, 1, 1);
    new Primitive("lcm", 64, 0, n);
    new Primitive("gcd", 63, 0, n);
    new Primitive("modulo", 62, 2, 2);
    new Primitive("remainder", 61, 2, 2);
    new Primitive("quotient", 60, 2, 2);
    new Primitive("abs", 59, 1, 1);
    new Primitive("/", 58, 2, 2);
    new Primitive("-", 57, 1, 2);
    new Primitive("*", 56, 0, n);
    new Primitive("+", 55, 0, n);
    new Primitive("min", 54, 1, n);
    new Primitive("max", 53, 1, n);
    new Primitive("even?", 52, 1, 1);
    new Primitive("odd?", 51, 1, 1);
    new Primitive("negative?", 50, 1, 1);
    new Primitive("positive?", 49, 1, 1);
    new Primitive("zero?", 48, 1, 1);
    new Primitive(">=", 47, 1, n);
    new Primitive("<=", 46, 1, n);
    new Primitive(">", 45, 1, n);
    new Primitive("<", 44, 1, n);
    new Primitive("=", 43, 1, n);
    new Primitive("inexact?", 42, 1, 1);
    new Primitive("exact?", 41, 1, 1);
    new Primitive("integer?", 40, 1, 1);
    new Primitive("rational?", 40, 1, 1);
    new Primitive("real?", 39, 1, 1);
    new Primitive("complex?", 39, 1, 1);
    new Primitive("number?", 39, 1, 1);
    new Primitive("string->symbol", 38, 1, 1);
    new Primitive("symbol->string", 37, 1, 1);
    new Primitive("symbol?", 36, 1, 1);
    new Primitive("assoc", 35, 2, 2);
    new Primitive("assv", 34, 2, 2);
    new Primitive("assq", 33, 2, 2);
    new Primitive("member", 32, 2, 2);
    new Primitive("memv", 31, 2, 2);
    new Primitive("memq", 30, 2, 2);
    new Primitive("list-ref", 29, 2, 2);
    new Primitive("list-tail", 28, 2, 2);
    new Primitive("reverse", 27, 1, 1);
    new Primitive("append", 26, 0, n);
    new Primitive("length", 25, 1, 1);
    new Primitive("list", 24, 0, n);
    new Primitive("list?", 23, 1, 1);
    new Primitive("null?", 22, 1, 1);
    new Primitive("cddddr", 21, 1, 1);
    new Primitive("cdddar", 21, 1, 1);
    new Primitive("cddadr", 21, 1, 1);
    new Primitive("cddaar", 21, 1, 1);
    new Primitive("cdaddr", 21, 1, 1);
    new Primitive("cdadar", 21, 1, 1);
    new Primitive("cdaadr", 21, 1, 1);
    new Primitive("cdaaar", 21, 1, 1);
    new Primitive("cadddr", 21, 1, 1);
    new Primitive("caddar", 21, 1, 1);
    new Primitive("cadadr", 21, 1, 1);
    new Primitive("cadaar", 21, 1, 1);
    new Primitive("caaddr", 21, 1, 1);
    new Primitive("caadar", 21, 1, 1);
    new Primitive("caaadr", 21, 1, 1);
    new Primitive("caaaar", 21, 1, 1);
    new Primitive("cdddr", 21, 1, 1);
    new Primitive("cddar", 21, 1, 1);
    new Primitive("cdadr", 21, 1, 1);
    new Primitive("cdaar", 21, 1, 1);
    new Primitive("caddr", 21, 1, 1);
    new Primitive("cadar", 21, 1, 1);
    new Primitive("caadr", 21, 1, 1);
    new Primitive("caaar", 21, 1, 1);
    new Primitive("cddr", 21, 1, 1);
    new Primitive("cdar", 21, 1, 1);
    new Primitive("cadr", 21, 1, 1);
    new Primitive("caar", 21, 1, 1);
    new Primitive("tenth", 20, 1, 1);
    new Primitive("ninth", 19, 1, 1);
    new Primitive("eighth", 18, 1, 1);
    new Primitive("seventh", 17, 1, 1);
    new Primitive("sixth", 16, 1, 1);
    new Primitive("fifth", 15, 1, 1);
    new Primitive("fourth", 14, 1, 1);
    new Primitive("third", 13, 1, 1);
    new Primitive("second", 12, 1, 1);
    new Primitive("set-cdr!", 11, 2, 2);
    new Primitive("set-car!", 10, 2, 2);
    new Primitive("rest", 9, 1, 1);
    new Primitive("cdr", 9, 1, 1);
    new Primitive("first", 8, 1, 1);
    new Primitive("car", 8, 1, 1);
    new Primitive("cons", 7, 2, 2);
    new Primitive("pair?", 6, 1, 1);
    new Primitive("equal?", 5, 2, 2);
    new Primitive("eq?", 4, 2, 2);
    new Primitive("eqv?", 3, 2, 2);
    new Primitive("boolean?", 2, 1, 1);
    new Primitive("not", 1, 1, 1);
    String derived = "(begin (set! null #null) (set! define (set-procedure-name! (macro (var . body) (if (pair? var) (list 'set! (first var) (list 'set-procedure-name! (cons 'lambda (cons (rest var) body)) (list 'quote (first var)))) (cons 'set! (cons var body)))) 'define)) (define cond (set-procedure-name! (macro clauses (define (process-clause clause else-part) (if (not (pair? clause)) (error '(bad cond clause:) clause) (if (null? (rest clause)) (list 'or (first clause) else-part) (if (eq? (second clause) '=>) ((lambda (tempvar) (list (list 'lambda (list tempvar) (list 'if tempvar (list (third clause) tempvar) else-part)) (first clause))) (string->symbol \"temp var\")) (if (member (first clause) '(#t else)) (cons 'begin (rest clause)) (list 'if (first clause) (cons 'begin (rest clause)) else-part)))))) (if (null? clauses) #f (process-clause (first clauses) (cons 'cond (rest clauses))))) 'cond)) (define tryCatch (set-procedure-name! (macro args (list 'jsint.Procedure.tryCatch (list 'lambda () (first args)) (second args))) 'tryCatch)) (define and (set-procedure-name! (macro args (cond ((null? args) #t) ((null? (rest args)) (list 'U.and1 (first args))) (else (list 'if (first args) (cons 'and (rest args)) #f)))) 'and)) (define quasiquote (set-procedure-name! (macro (x) (define (constant? exp) (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp)))) (define (combine-skeletons left right exp) (cond ((and (constant? left) (constant? right)) (if (and (eqv? (eval left) (car exp)) (eqv? (eval right) (cdr exp))) (list 'quote exp) (list 'quote (cons (eval left) (eval right))))) ((null? right) (list 'list left)) ((and (pair? right) (eq? (car right) 'list)) (cons 'list (cons left (cdr right)))) (else (list 'cons left right)))) (define (expand-quasiquote exp nesting) (cond ((vector? exp) (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting))) ((not (pair? exp)) (if (constant? exp) exp (list 'quote exp))) ((and (eq? (car exp) 'unquote) (= (length exp) 2)) (if (= nesting 0) (second exp) (combine-skeletons ''unquote (expand-quasiquote (cdr exp) (- nesting 1)) exp))) ((and (eq? (car exp) 'quasiquote) (= (length exp) 2)) (combine-skeletons ''quasiquote (expand-quasiquote (cdr exp) (+ nesting 1)) exp)) ((and (pair? (car exp)) (eq? (caar exp) 'unquote-splicing) (= (length (car exp)) 2)) (if (= nesting 0) (list 'append (second (first exp)) (expand-quasiquote (cdr exp) nesting)) (combine-skeletons (expand-quasiquote (car exp) (- nesting 1)) (expand-quasiquote (cdr exp) nesting) exp))) (else (combine-skeletons (expand-quasiquote (car exp) nesting) (expand-quasiquote (cdr exp) nesting) exp)))) (expand-quasiquote x 0)) 'quasiquote)) (define let (set-procedure-name! (macro (bindings . body) (define (varval v) (string->symbol (string-append v \"=\"))) (define (named-let name bindings body) ((lambda (new-bindings) `(let ,(cons `(,name #f) new-bindings) (set! ,name (lambda ,(map first bindings) unquote body)) (,name unquote (map car new-bindings)))) (map (lambda (b) `(,(varval (car b)) ,(cadr b))) bindings))) (if (symbol? bindings) (named-let bindings (first body) (rest body)) `((lambda ,(map first bindings) unquote body) unquote (map second bindings)))) 'let)) (define let* (set-procedure-name! (macro (bindings . body) (if (null? bindings) (jsint.Scheme.toBody body) (if (null? (cdr bindings)) `(let (,(first bindings)) unquote body) `(let (,(first bindings)) (let* ,(rest bindings) unquote body))))) 'let*)) (define letrec (set-procedure-name! (macro (bindings . body) (let ((vars (map first bindings)) (vals (map second bindings))) `(let ,(map (lambda (var) `(,var #f)) vars) ,@(map (lambda (var val) `(set! ,var ,val)) vars vals) ,(jsint.Scheme.toBody body)))) 'letrec)) (define case (set-procedure-name! (macro (exp . cases) (let ((tempvar (string->symbol \"temp var\"))) (define (do-case case) (cond ((not (pair? case)) (error '(bad syntax in case:) case)) ((eq? (first case) 'else) case) (else `((member ,tempvar ',(first case)) unquote (rest case))))) `(let ((,tempvar ,exp)) (cond unquote (map do-case cases))))) 'case)) (define do (set-procedure-name! (macro (bindings test-and-result . body) (let ((variables (map first bindings)) (inits (map second bindings)) (steps (map (lambda (clause) (if (null? (cddr clause)) (first clause) (third clause))) bindings)) (result (if (null? (cdr test-and-result)) ''unspecified `(begin unquote (cdr test-and-result))))) (let ((tempvar '<loop>)) `(letrec ((,tempvar (lambda ,variables (if ,(first test-and-result) ,result (begin ,@body (,tempvar unquote steps)))))) (,tempvar unquote inits))))) 'do)) (define delay (set-procedure-name! (macro (exp) (define (make-promise proc) (let ((result-ready? #f) (result #f)) (lambda () (if result-ready? result (let ((x (proc))) (if result-ready? result (begin (set! result-ready? #t) (set! result x) result))))))) `(,make-promise (lambda () ,exp))) 'delay)) (define time (set-procedure-name! (macro (exp . ntimes) `(time-call (lambda () ,exp) ,(if (pair? ntimes) (car ntimes) 1))) 'time)) (define define-macro (set-procedure-name! (macro (spec . body) (if (pair? spec) `(define ,(first spec) (set-procedure-name! (macro ,(rest spec) unquote body) ',(first spec))) `(define ,spec (set-procedure-name! (macro ,(second (first body)) ,@(rest (rest (first body)))) ',spec)))) 'define-macro)) (define (missing-classes classes sofar) (if (null? classes) sofar (missing-classes (cdr classes) (if (eq? (class (car classes)) #null) (cons (car classes) sofar) sofar)))) (define-macro (if-classes classes then else) (if (null? (missing-classes classes '())) then else)) (define-macro (when-classes classes . then) `(if-classes ,classes (begin ,@then) #f)) (define-macro (class-case varlist . clauses) (define (runtimeClassName c) (string->symbol (string-append (.getName (class c)) \".class\"))) (define (instanceof v c) `(.isInstance ,(runtimeClassName c) ,v)) `(cond ,@(map (lambda (clause) (if (equal? (first clause) 'else) clause `((and ,@(map instanceof varlist (first clause))) ,@(rest clause)))) clauses))) (define (define-method-runtime name type-names f name-args) (let ((missing (missing-classes type-names '()))) (if (null? missing) (jsint.Generic.defineMethod name type-names f) (jsint.E.warn (string-append \"Can't define-method \" name-args \" classes \" missing \" do not exist.\"))))) (define define-method (macro (name-args . body) (define (arg-name x) (if (pair? x) (car x) x)) (define (maybe-second x default) (if (and (pair? x) (pair? (cdr x))) (cadr x) default)) (define (arg-type x) (maybe-second x 'java.lang.Object)) (let* ((name (car name-args)) (args (cdr name-args)) (arg-types (map arg-type args))) `(define-method-runtime ',name ',arg-types (lambda ,(map arg-name args) ,@body) ',name-args)))) (define package (macro args #t)) (define (array a-class . args) (let ((v (make-array a-class (length args)))) (let loop ((i 0) (as args)) (if (null? as) v (begin (vector-set! v i (car as)) (loop (+ i 1) (cdr as))))))) (define (make-array a-class size) (java.lang.reflect.Array.newInstance a-class size)) (define (!{} . args) (let loop ((args args) (sb (StringBuffer.))) (cond ((null? args) (.toString sb)) ((pair? (car args)) (loop (cons (car (car args)) (cons (cdr (car args)) (cdr args))) sb)) ((null? (car args)) (loop (cdr args) sb)) (else (.append sb (U.stringify (car args) #f)) (loop (cdr args) sb))))) (define !#{} !{}) (define (string-set! s i v) (.hash$# s 0) (vector-set! (.value$# s) i v)) (define (string-fill! s x) (.hash$# s 0) (let ((L (string-length s)) (v (.value$# s))) (let loop ((i 0)) (if (< i L) (begin (vector-set! v i x) (loop (+ i 1))))) s)) (define (string-copy s) (.toString (StringBuffer. s))) (define use-module (lambda (filename . R) (case (length R) ((0) (use-module filename 'import 'all #f)) ((1) (use-module filename (first R) 'all #f)) ((2) (use-module filename (first R) (second R) #f)) (else (let* ((specifier (first R)) (symbols (second R)) (prefix (third R)) (symarray (if (or (equal? symbols #null) (equal? symbols 'all)) #null (list->array jsint.Symbol.class symbols)))) (case specifier ((import-procedures) (.environmentImport (Scheme.currentEvaluator) filename prefix #f symarray)) ((import-macros) (.environmentImport (Scheme.currentEvaluator) filename #f #t symarray)) ((import) (.environmentImport (Scheme.currentEvaluator) filename prefix #f symarray) (.environmentImport (Scheme.currentEvaluator) filename #f #t symarray)) (else (error (!{} \"unknown specifier \" specifier \" in (use-module \" filename \" \" specifier \" \" symbols \" \" prefix \")\\n\"))))))))))";
    //Scheme.load("jsint/primproc.scm");
    Scheme.load(new InputPort(new java.io.StringReader(derived)));
    primitives_loaded=true;
    }

    static boolean primitives_loaded = false;}
