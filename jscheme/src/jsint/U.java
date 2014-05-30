package jsint;

import java.io.*;
import java.lang.reflect.Array;

/**
   A class to hold static utility methods; the name "U" stands for
   "Utility", but is short because it will be used a lot.
   @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
   subsequently modified by Jscheme project members
   licensed under zlib licence (see license.txt)
**/

public abstract class U {

  /** a flag which specifies whether Java (or Scheme) syntax
   * should be used when printing Scheme terms 
   **/
  public static boolean useJavaSyntax=true;

  //////////////// Constants ////////////////

  /** Same as Boolean.TRUE. **/
  public static final Boolean TRUE = Boolean.TRUE;

  /** Same as Boolean.FALSE. **/
  public static final Boolean FALSE = Boolean.FALSE;

  /** The value to return when a variable is not defined. **/
  public static final Symbol UNDEFINED = Symbol.intern("#!undefined");
  
  /** The value to use when a parameter is not supplied to a procedure. **/
  // public static final Pair MISSING = Pair.EMPTY;
  public static final Symbol MISSING = Symbol.intern("#!missing");
  /** An argument list with zero arguments. **/
  public static final Object[] NO_ARGS = new Object[0];
  public static final Object[] EMPTY_ARGS = new Object[]{Pair.EMPTY};
  
  //////////////// Conversion Routines ////////////////

  // The following convert or coerce objects to the right type.

  /** #f and #null are treated as false **/
  public static boolean isFalse(Object x) {
    return x == null || FALSE.equals(x) ;
  }

  /** One argument and used by (and) macro. **/
  public static Object and1(Object x) {
    return isFalse(x) ? FALSE : x;
  }

  /** Convert Scheme object to boolean. **/
  public static boolean to_bool(Object x) {
    return !(isFalse(x)); }

  /** Convert boolean to Boolean. **/
  public static Boolean toBool(boolean x) { return x ? TRUE : FALSE; }

  /** Convert Scheme object to Boolean.  **/
  public static Boolean toBool(Object x) {
    return isFalse(x) ? FALSE : TRUE;
  }

  /** Returns TRUE if x is FALSE or null. **/
  public static Boolean not(Object x) {
    return isFalse(x) ? TRUE : FALSE;
  }
  /** Converts a Character to a char, or calls error for non-Characters. **/
  public static char to_char(Object x) {
    if (x instanceof Character) return ((Character)x).charValue();
    else return to_char(E.typeError("char", x));
  }

  /** Converts a Character to a lowercase char, or calls error for non-Characters. **/
  public static char to_lc_char(Object x) {
    if (x instanceof Character) 
      return Character.toLowerCase(((Character)x).charValue());
    else return to_lc_char(E.typeError("char", x));
  }

  /** Number of chars, positive, negative ints to cache. **/
  private final static int NUM_CACHED = 128;
  private final static Character[] cachedCharacters = new Character[128];

  /** Converts a char to a Character. Caches low-numbered chars. **/
  public static Character toChar(char ch) {
    if (ch < NUM_CACHED) {
      Character c = cachedCharacters[ch];
      return (c != null) ? c : (cachedCharacters[ch] = new Character(ch));
    } else return new Character(ch);
  }
  
  public static Class toClass(Object c) {
    if (c instanceof Class) return (Class) c;
    else return Import.classNamed(stringify(c, false));
  }
      
  public static Class maybeToClass(Object c) {
    if (c instanceof Class) return (Class) c;
    else return Import.maybeClassNamed(stringify(c, false));
  }
      
  private final static Integer[] cachedPosInts = new Integer[NUM_CACHED];
  private final static Integer[] cachedNegInts = new Integer[NUM_CACHED];

  /** Convert int to Integer. Caches small ints so that we only ever
   * make one copy of new Integer(0), new Integer(1), etc. **/
  public static Integer toNum(int i) {
    if (i >= 0) {
      if (i < NUM_CACHED) {
	Integer in = cachedPosInts[i];
	return (in != null) ? in : (cachedPosInts[i] = new Integer(i));
      } else return new Integer(i);
    } else if (i > -NUM_CACHED) {
      Integer in = cachedNegInts[-i];
      return (in != null) ? in : (cachedNegInts[-i] = new Integer(i));
    } return new Integer(i);
  }
  
  /** Convert long to Number, either Integer or Long. **/
  public static Number toNum(long i) {
    if (i <= Integer.MAX_VALUE && i >= Integer.MIN_VALUE) return toNum((int)i);
    else return new Long(i);
  }

  /** A Double with value 0.0. Defined here so that we need only one. **/
  public static final Double ZERO = new Double(0.0);

  /** A Double with value 1.0. Defined here so that we need only one. **/
  public static final Double ONE = new Double(1.0);

  /** Convert double to Double. Caches 0 and 1; makes new for others. **/
  public static Double toNum(double x) { 
    return (x == 0.0) ? ZERO : (x == 1.0) ? ONE : new Double(x); }

  /** Converts a Scheme object to a double, or calls error. **/
  public static double toReal(Object x) { 
    if (x instanceof Number) return ((Number)x).doubleValue();
    else return toReal(E.typeError("real number", x));
  }

  /** Converts a Scheme object to an int, or calls error. **/
  public static int toInt(Object x) {
    try {
      return ((Number)x).intValue(); 
    } catch(ClassCastException e) {
      return toInt(E.typeError("integer", x));
    }
  }

  /** Converts a Scheme object to an int, return the default
   * if it is not possible to convert the object to an int. **/
  public static int toInt(Object x, int defaultVal) {
    if (x instanceof Number) return ((Number)x).intValue();
    else return defaultVal;
  }

  /** Cast a Scheme object to a String, or call error. **/
  public static String toStr(Object x) {
    return (x instanceof String) ? (String)x :
      toStr(E.typeError("string", x));
  }

  /** Cast a Scheme object to a Scheme symbol, or call error. **/
  public static Symbol toSym(Object x) {
    if (x instanceof Symbol) return (Symbol)x;
    else return toSym(E.typeError("symbol", x)); 
  }

  /** Cast a Scheme object to a procedure, or call error. **/
  public static Procedure toProc(Object x) {
    try {
      return (Procedure) x; 
    } catch (ClassCastException e) {
      return toProc(E.typeError("procedure", x));
    }
  }

  /** Check if the argument is a non-empty list. **/
  public static boolean isPair(Object x) { 
    return x instanceof Pair && x != Pair.EMPTY; 
  }
  
  /** Cast a Scheme object to a Pair (can't be the empty list). **/
  public static Pair toPair(Object x) {
    try {
      if (x != Pair.EMPTY) return (Pair) x; 
      else return toPair(E.typeError("pair(i.e. non-empty list)", x));
    } catch (ClassCastException e) {
      return toPair(E.typeError("pair(i.e. non-empty list)", x));
    }
      /*
    if (x != Pair.EMPTY && x instanceof Pair) return (Pair) x;
    else return toPair(E.typeError("pair(i.e. non-empty list)", x));
      */
  }

  /** Cast a Scheme object to a Pair or the empty list. **/
  public static Pair toList(Object x) {
    try { return (Pair) x;}
    catch (ClassCastException e) {
      return toPair(E.typeError("list (i.e. pair or empty)", x));
    }
  }

  /** Cast a Scheme object to a Scheme input port, which is an InputPort.
   * If the argument is missing, returns Scheme.getInput(). **/
  public static InputPort toInPort(Object x) {
    if (x == MISSING) return Scheme.currentEvaluator().getInput();
    else if (x instanceof InputPort) return (InputPort)x;
    else return toInPort(E.typeError("input port", x)); 
  }

  /** Cast a Scheme object to a Scheme input port, which is a PrintWriter.
   * If the argument is missing, returns Scheme.getOutput(). **/
  public static PrintWriter toOutPort(Object x) {
    if (x == MISSING) return Scheme.currentEvaluator().getOutput();
    else if (x instanceof PrintWriter) return (PrintWriter)x;
    else return toOutPort(E.typeError("output port", x)); 
  }

  //////////////// Basic manipulation Routines ////////////////

  /** Return the first element of a Pair, or error. **/
  public static Object first(Object x) {
    return toPair(x).first;
  }

  /** Return the rest of a Pair, or error. **/
  public static Object rest(Object x) {
    return toPair(x).rest;
  }

  /** Return the second element of a list. **/
  public static Object second(Object x) {
    return toPair(x).second();
  }

  /** Creates a three element list. **/
  public static Pair list(Object a, Object b, Object c) {
    return new Pair(a, new Pair(b, new Pair(c, Pair.EMPTY)));
  }

  /** Creates a two element list. **/
  public static Pair list(Object a, Object b) {
    return new Pair(a, new Pair(b, Pair.EMPTY));
  }

  /** Creates a one element list. **/
  public static Pair list(Object a) {
    return new Pair(a, Pair.EMPTY);
  }

  /**  Structural equality. **/
  public static boolean equal(Object x, Object y) {
    if (x == null || y == null) return x == y;
    else if (x == Pair.EMPTY || y == Pair.EMPTY) return x == y;
    else if (x instanceof Object[]) {
      if (!(y instanceof Object[])) return false;
      Object[] xo = (Object[])x, yo = (Object[])y;
      if (xo.length != yo.length) return false;
      for (int i = xo.length - 1; i >= 0; i--)
        if (!equal(xo[i], yo[i])) return false;
      return true;
    } else return (x.equals(y) || eqv(x,y)); 
  }

//   public static boolean eqv(Object x, Object y) {
//     try {
//     return x == y 
//       || (x instanceof Number && y instanceof Number &&
// 	  ((x instanceof Integer || x instanceof Long || x instanceof Short || x instanceof Byte) &&
//            (((Number)x).longValue()== ((Number) y).longValue()))
// 	  || 
// 	  ((x instanceof Float || x instanceof Double) &&
//            (((Number)x).doubleValue()== ((Number) y).doubleValue())))
//       || (x instanceof Character && x.equals(y))
//       || (x instanceof Boolean && x.equals(y));
//     } catch (ClassCastException e) { return false; }
//   }
 
  /** Atomic equality. **/
  public static boolean eqv(Object x, Object y) {
    return x == y ||
      x == null && y == null ||
      x != null && y != null &&
      sameAtomicClasses(x.getClass(),y.getClass()) && x.equals(y);
  }
 
  private static boolean sameAtomicClasses (Class cx, Class cy) {
    return cx == cy &&
      (cx.getSuperclass() == Number.class ||
       cx == Symbol.class ||
       cx == Character.class ||
       cx == Boolean.class);
  }

  /** Write the object to a port.  If quoted is true, use "str" and #\c,
   * otherwise use str and c. **/
  public static Object write(Object x, PrintWriter port, boolean quoted) {
    port.print(stringify(x, quoted)); 
    port.flush();
    return x;
  }
        

  /** Check that the form has between min and max arguments (exclusive
   * of the first element of the form). If the form has the wrong
   * number of arguments, then complain. **/
   public static boolean checkNargs(int min, int max, int given, Object form) {
     if (given >= min && given <= max) return true;
     else {
      E.warn("expected " + min +
           (min == max ? "" : (max == Integer.MAX_VALUE) ? " or more" : (" to " + max))
            + " arguments, but got " + given, form);
      return false;
     }
   }

  //////////////// Output ////////////////

  /** Convert a Scheme object to its printed representation, as a java
   * String. If quoted is true, use "str" and #\c, otherwise use str and
   * c. You need to pass in a StringBuffer that is used to accumulate the
   * results. (If the interface didn't work that way, the system would use
   * lots of little internal StringBuffers.  But note that you can still call
   * <tt>stringify(x)</tt> and a new StringBuffer will be created for you. 
   * If useJavaSyntax is true, then literals are printed using a Java syntax
   **/

  public static StringBuffer stringify(Object x, boolean quoted, StringBuffer buf) { 
    if (x == Pair.EMPTY) { buf.append("()");
    } else if (x == null) { buf.append("#null");
    } else if (x instanceof Boolean) { if (Boolean.TRUE.equals(x)) buf.append("#t"); else buf.append("#f");
    } else if (x == TRUE) { buf.append("#t");
    } else if (x == FALSE) { buf.append("#f");
    } else if (x instanceof Pair) { ((Pair)x).stringifyPair(quoted, buf);
    } else if (x instanceof Character) { 
      char ch = ((Character)x).charValue();
      if (ch=='\'') 
          buf.append(quoted? (useJavaSyntax?  "#'\\\'\'" : "#\\'") : "'");
      else if (useJavaSyntax) {
	  if (quoted) buf.append("#'");
          stringifyChar(buf,ch,quoted);
	  if (quoted) buf.append("'");
      }
      else {
        if (quoted) buf.append("#\\");
        if (quoted && (ch == ' ' || ch == '\n'))
          buf.append((ch == ' ') ? "space" : "newline");
        else buf.append(ch);}
    } else if (x instanceof String) { // string
      String s = (String)x;
      if (quoted) buf.append('"');
      if (useJavaSyntax)
        for (int i = 0; i < s.length(); i++) 
	    stringifyChar(buf,s.charAt(i),quoted);
      else
        for (int i = 0; i < s.length(); i++) {
          if (quoted && (s.charAt(i) == '"' || s.charAt(i) == '\\')) 
            buf.append('\\');
          buf.append(s.charAt(i));
        }

      if (quoted) buf.append('"');
    } else if (x instanceof Object[]) { // vector
        Object[] v = (Object[])x;
        buf.append("#(");
        for (int i=0; i<v.length; i++) {
            stringify(v[i], quoted, buf);
            if (i != v.length-1) buf.append(' ');
        }
        buf.append(')');
    } else if (x instanceof Number) { // number
        buf.append(x);
        if ((x instanceof Integer) || (x instanceof Double)) /* do nothing */;
        else if (x instanceof Byte) buf.append("B");
        else if (x instanceof Short) buf.append("S");
        else if (x instanceof Long) buf.append("L");
        else if (x instanceof Float) buf.append("F");       
    } else {
      buf.append(x);
    }
    return buf;
  }

  private static void stringifyChar(StringBuffer buf,char ch,boolean quoted) {
	  switch(ch) {
	  case '\b': buf.append(quoted?"\\b":"\b"); break;
	  case '\t': buf.append(quoted?"\\t":"\t"); break;
	  case '\n': buf.append(quoted?"\\n":"\n"); break;
	  case '\f': buf.append(quoted?"\\f":"\f"); break;
	  case '\r': buf.append(quoted?"\\r":"\r"); break;
	  case '\"': buf.append(quoted?"\\\"":"\""); break;
	  case '\\': buf.append(quoted?"\\\\":"\\"); break;
          default: buf.append(ch);
	  }
  }

  /** Convert x to a String giving its external representation. 
   * Strings and characters are quoted. **/
  public static String stringify(Object x) { return stringify(x, true); }

  /** Convert x to a String giving its external representation. 
   * Strings and characters are quoted iff <tt>quoted</tt> is true.. **/
  public static String stringify(Object x, boolean quoted) { 
    // Handle these cases without consing:
    if (x instanceof String && !quoted) return ((String) x);
    else if (x instanceof Symbol) return ((Symbol) x).toString();
    else return stringify(x, quoted, new StringBuffer()).toString();
  }
	       
  //////////////// Various ////////////////
  
  public static String makeString(int size, Object fill) {
    char[] chars = new char[size];
    if (fill != MISSING) {
      char ch = to_char(fill);
      for (int i = 0; i < size; i++) chars[i] = ch;
    }
    return new String(chars);
  }
  	       
  public static String stringAppend(Pair args) {
    if (args == Pair.EMPTY) return "";
    StringBuffer result = new StringBuffer();
    while (args != Pair.EMPTY) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      result.append(stringify(args.first, false));
      args = toList(args.rest);
    }
    return result.toString();
  }

  public static Object memberAssoc(Object obj, Object list, boolean member, int eq) {
    boolean found = false;
    while (isPair(list)) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      Object target = (member) ? first(list) : first(first(list));
      switch (eq) {
      case 1: found = (target == obj); break;
      case 2: found = eqv(target, obj); break;
      case 3: found = equal(target, obj); break;
      default: E.warn("Bad option to memberAssoc:" + eq); return FALSE;
      }
      if (found) return (member) ? list : first(list);
      list = rest(list);
    }
    return FALSE;
  }

  /** Compute (x op arg1 op arg2 op ...), in ints or doubles. **/
  public static Object numCompute(Object x, Pair args, char op) {
    return (x instanceof Integer) ? numCompute(toInt(x), args, op) 
	                          : numCompute(toReal(x), args, op);
  }

  /** Compute (result op arg1 op arg2 op ...). Return the result
  * as an Object, but along the way, use result (a long) as an accumulator of
  * the result so far. For compare ops, returns FALSE for false, and
  * a number for true. **/ 
  public static Object numCompute(long result, Pair args, char op) {
    for (; isPair(args); args = toList(args.rest)) {
      if (!(args.first instanceof Integer))
        return numCompute((double)result, args, op);
      long y = toInt(args.first);
      switch (op) {
      case '>': if (!(result >  y)) return FALSE; else result = y; break;
      case '<': if (!(result <  y)) return FALSE; else result = y; break;
      case '=': if (!(result == y)) return FALSE; else result = y; break;
      case 'L': if (!(result <= y)) return FALSE; else result = y; break;
      case 'G': if (!(result >= y)) return FALSE; else result = y; break;
      case 'X': if (y > result) result = y; break; // max
      case 'N': if (y < result) result = y; break; // min
      case '+': result += y; break;
      case '-': result -= y; break;
      case '*': result *= y; break;
      case '/': if (result % y == 0) result /= y; 
                else return numCompute((double)result, args, op);
                break;
      default:  return E.error("internal error: unrecognized op: " + op);
      }
      // If overflow ints, move to doubles
      if (result < Integer.MIN_VALUE || result > Integer.MAX_VALUE)
        return numCompute((double)result, args, op);
    }
    return toNum(result);
  }

  /** Compute (result op arg1 op arg2 op ...). Return the result
  * as an Object, but along the way, use result (a double) as an accumulator of
  * the result so far. For compare ops, returns FALSE for false, and
  * a number for true. **/ 
  public static Object numCompute(double result, Pair args, char op) {
    for (; isPair(args); args = toList(args.rest)) {
      double y = toReal(args.first);
      switch (op) {
      case '>': if (!(result >  y)) return FALSE; else result = y; break;
      case '<': if (!(result <  y)) return FALSE; else result = y; break;
      case '=': if (!(result == y)) return FALSE; else result = y; break;
      case 'L': if (!(result <= y)) return FALSE; else result = y; break;
      case 'G': if (!(result >= y)) return FALSE; else result = y; break;
      case 'X': if (y > result) result = y; break; // max
      case 'N': if (y < result) result = y; break; // min
      case '+': result += y; break;
      case '-': result -= y; break;
      case '*': result *= y; break;
      case '/': result /= y; break;
      default:  return E.error("internal error: unrecognized op: " + op);
      }
    }
    return toNum(result);
  }

  public static Object numberToString(Object x, Object y) {
    int base = (y instanceof Number) ? toInt(y) : 10;
    if (base != 10 && x instanceof Integer) { // An integer
      return Long.toString(toInt(x), base);
    } else { // A floating point number
      return x.toString();
    }
  }

  public static Object stringToNumber(Object x, Object y) {
    return InputPort.schemeStringToNumber
      (stringify(x, false), (y instanceof Number) ? toInt(y) : 10);
  }

  public static Object stringToList(Object x) {
    Pair result = Pair.EMPTY;
    String str = toStr(x);
    for (int i = str.length()-1; i >= 0; i--)
      result = new Pair(toChar(str.charAt(i)), result);
    return result;
  }



  /** Convert a list of characters to a String. **/
  public static String listToString(Object chars) {
    char[] str = new char[toList(chars).length()];
    for (int i = 0; isPair(chars); i++) {
      str[i] = to_char(first(chars));
      chars = rest(chars);
    }
    return new String(str);
  }
  
  /** Return <0 if x is alphabetically first, >0 if y is first,
   * 0 if same.  Case insensitive iff ci is true.  Error if not strings. 
   * NOTE: In Java 2, just use String.CompareIgnorecase.  But that
   * method is missing in Java 1.0 and 1.1. **/
  public static int stringCompareIgnoreCase(Object x, Object y) {
    String xs = toStr(x), ys = toStr(y);
    for (int i = 0; i < xs.length(); i++) {
      int diff = Character.toUpperCase(xs.charAt(i)) 
               - Character.toUpperCase(ys.charAt(i));
      if (diff != 0) return diff;
    }
    return xs.length() - ys.length();
  }
  
  public static long gcd(Pair args) {
    return (args.rest == Pair.EMPTY) ? toInt(args.first)
      : gcd(Math.abs(toInt(args.first)), gcd((Pair)args.rest));
  }

  static long gcd(long a, long b) { return (b == 0) ? a : gcd(b, a % b); }

  static long lcm(Object args) {
    long L = 1, g = 1;
    while (isPair(args)) {
      long n = Math.abs((long)toInt(first(args)));
      g = gcd(n, L);
      L = (g == 0) ? g : (n / g) * L;
      args = toList(rest(args));
    }
    return L;
  }

  public static PrintWriter openOutputFile(Object filename) {
    try {
      return new PrintWriter(new FileWriter(stringify(filename, false)));
    } catch (FileNotFoundException e) {
      return (PrintWriter)E.error(e.toString());
    } catch (IOException e) {
      return (PrintWriter)E.error("IOException: " + e);
    }
  }

  /** Opens a file, resource, or URL. Returns null if unsuccessful.**/
  public static InputPort openInputFile(Object filename) {
    return Scheme.open(toStr(filename));
  }

  public static Object callWithInputFile (Object filename, Procedure proc) {
    InputPort in = null;
    Object result = null;
    try {
      in = openInputFile(filename);
      if (in == null) E.error("could not find '" + filename +
			      "' as a resource URL, or File.");
      result = proc.apply(list(in));
    } finally { if (in != null) in.close(); }
    return result;
  }
  
  public static Object callWithOutputFile(Object filename, Procedure proc) {
    PrintWriter out = null;
    Object result = null;
    try { out = openOutputFile(filename);
      result = proc.apply(list(out));
    } finally { if (out != null) out.close(); }
    return result;
  }
  
				 
  /** Return true if x is a proper list: null-terminated and finite.
   * Return false if it is an infinite or non-null-terminated list.
   * Not to be confused with <tt>(or (pair? x) (null? x))</tt>. **/
  public static boolean isList(Object x) {
    Object slow = x, fast = x;
    for(;;) {
      if (fast == Pair.EMPTY) return true;
      if (!isPair(fast) || !isPair(slow) || slow == rest(fast)) return false;
      slow = rest(slow);
      fast = rest(fast);
      if (fast == Pair.EMPTY) return true;
      if (!isPair(fast)) return false;
      fast = rest(fast);
    }
  }

  /* Original not tail recursive version.
  public static Object append(Pair args) {
    if (isPair(args)) 
      if (isPair(args.rest))
	return append(args.first, append(toList(args.rest)));
      else return args.first;
    else return args;
  }

  public static Object append(Object x, Object y) {
    return isPair(x) ? new Pair(first(x), append(rest(x), y)) : y;
  }
  Original not tail recursive version. */

  /** args is a list of lists to be appended together. **/
  public static Object append(Object args) {
    if (isPair(args)) {
      Queue queue = new Queue();
      while (isPair(rest(args))) {
	if (Scheme.isInterruptable()) Scheme.interruptCheck();
	for (Object x = first(args); isPair(x); x = rest(x))
	  queue.add(first(x));
	args = rest(args);
      }
      queue.getLast().rest = first(args);
      return queue.getContent();
    } else return args;
  }
  /*
    ;;; Append unit tests:
    (define (grow n)
      (if (<= n 0) (list (list 1))
          (let ((L (grow (- n 1))))
            (append L L))))

	;;; Original version:
	(length (apply append (grow 13)))	; -> 8192
	(length (apply append (grow 14)))	; Stack overflow.

	;;; New version:
	(length (apply append (grow 20)))       ; 1048576

	(assert (eqv? (append 3) 3))
	(assert (eqv? (append '() '() '()) '()))
	(assert (= (append 3 3) 3))
	(assert (equal? (append '(1) '() 2) '(1 . 2)))
	(assert (equal? (append '(1) '() '(2 3)) '(1 2 3)))
	(let ((x '(1 2 3))) (assert (eq? (append '() x) x)))
  */
  /** A continuation exception is a specially marked RuntimeException. **/
  public static Object callCC(Procedure k) {
    ContinuationException cc = new ContinuationException();
    Continuation proc = new Continuation(cc);
    try { return k.apply(list(proc)); }
    catch (ContinuationException e) { 
      if (e == cc) return proc.value;
      else throw e; 
    }
  }

  /** Map proc over a list of lists of args.
   * If result is a passed in as a Pair, then accumulate values there.  
   * Otherwise, just return the empty list. **/
  public static Pair map(Procedure proc, Object args, Pair result) {
    Pair end = result;
    if (rest(args) == Pair.EMPTY) { // One-argument map
      Object argList = first(args);
      while (isPair(argList)) {
	if (Scheme.isInterruptable()) Scheme.interruptCheck();
        Object x = proc.apply(list(first(argList)));
        if (end != Pair.EMPTY) end = (Pair) (end.rest = list(x)); 
        argList = rest(argList);
      }
    } else { // Multi-argument map
      Procedure car = toProc(Symbol.CAR.getGlobalValue()),
        cdr = toProc(Symbol.CDR.getGlobalValue());
      while  (isPair(first(args))) {
	if (Scheme.isInterruptable()) Scheme.interruptCheck();
        Object x = proc.apply(map(car, list(args), list(TRUE)));
        if (end != Pair.EMPTY) end = (Pair) (end.rest = list(x));
        args = map(cdr, list(args), list(TRUE));
      }
    }
    return ((U.isPair(result)) ? (Pair)rest(result) : result);
  }
  
  /** Call the procedure repeatedly nTimes, and return a list of the
   * the last result, the elapsed time, and the memory used. **/
  public static Pair timeCall(Procedure proc, int nTimes) {
    Runtime runtime = Runtime.getRuntime();
    runtime.gc();
    long startTime = System.currentTimeMillis();
    long startMem = runtime.freeMemory();
    Object ans = FALSE;
    for (int i = 0; i < nTimes; i++) {
      ans = proc.apply(Pair.EMPTY);
    }
    long time = System.currentTimeMillis() - startTime;
    long mem = startMem - runtime.freeMemory();
    return new Pair(ans, list(list(U.toNum(time), Symbol.intern("msec")),
                         list(U.toNum(mem), Symbol.intern("bytes"))));
  }
  
  /** Used for debugging. **/
  public static Object p(String x, Object y) {
    if (Symbol.intern("debug").getGlobalValue() == U.TRUE) {
      Scheme.currentEvaluator().getError().println(x + stringify(y));
    }
    return y; 
  }

  /** KRA 01OCT01: New vector semantics.
      <p>

      Any Java array is now treated as a Scheme vector.  Type tests
      are inlined to keep the same performance for (vector-length),
      (vector-ref) and (vector-set!) as the old code when accessing
      Object[]'s
   **/

  /** Cast a Scheme object to a Scheme vector, or call error. **/
  public static Object toVec(Object x) {
    return x instanceof Object[] ? x :
      x != null && x.getClass().isArray() ? x :
      E.typeError("vector", x);
  }

  public static boolean isVector (Object x) {
    return x instanceof Object[] || x != null && x.getClass().isArray();
  }

  public static Object makeVector (Object x) {return new Object[U.toInt(x)];}

  public static Object makeVector (Object x, Object fill) 
    {Object[] v = new Object[U.toInt(x)];
    for (int i=0; i<U.toInt(x); i++) v[i]=fill;
    return v;
    }

  public static Object vectorFill(Object vec, Object fill) {
    for (int i = Array.getLength(vec)-1; i >= 0; i--)
      Array.set(vec, i, fill);
    return U.UNDEFINED;
  }

  public static Object vectorLength (Object x) {
    return x instanceof Object[] ? U.toNum(((Object[])x).length) :
      x != null && x.getClass().isArray() ?
      U.toNum(Array.getLength(x)) :
      E.typeError("vector", x);
  }

  public static Object vectorRef (Object x, Object y) {
    return x instanceof Object[] ? ((Object[]) x)[U.toInt(y)] :
      x != null && x.getClass().isArray() ?
      Array.get(x, U.toInt(y)) :
      E.typeError("vector", x);
  }
    
  public static Object vectorSet (Object x, Object y, Object z) {
    if (x instanceof Object[]) return ((Object[]) x)[U.toInt(y)] = z;
    else if (x != null && x.getClass().isArray()) {
      Array.set(x, U.toInt(y), z);
      return z;
    } else return E.typeError("vector", x);
  }

  public static Pair vectorToList(Object vec) {
    Pair result = Pair.EMPTY;
    for (int i = Array.getLength(vec)-1; i>=0; i--) {
      result = new Pair(Array.get(vec, i), result);
    }
    return result;
  }
  
  public static Object[] listToVector(Object x) {
    Pair list = toList(x);
    int L = list.length();
    Object[] result = new Object[L];
    for (int i = 0; isPair(list); i++, list = toList(list.rest))
      result[i] = first(list);
    return result;
  }

  public static Object listToArray(Class C, Object x) {
    Pair list = toList(x);
    int L = list.length();
    if (L == 0) 
	return java.lang.reflect.Array.newInstance(C,0);
    else {
      Object result = java.lang.reflect.Array.newInstance(C,L);
      for (int i = 0; isPair(list); i++, list = toList(list.rest)) {
	java.lang.reflect.Array.set(result,i,first(list));
      }
      return result;
    }
  }
  
  public static Pair arrayToList(Object x) {
    Pair result = Pair.EMPTY;
    for (int i = Array.getLength(x)-1; i>=0; i--) {
      result = new Pair(Array.get(x,i), result);
    }
    return result;
  }

  /** R5RS apply as requested by
      "Hoehle, Joerg-Cyril" <Joerg-Cyril.Hoehle@t-systems.com>.
      We splice the last argument onto the end of the argument list.
  **/
  public static Object apply(Procedure p, Pair args) {
    Pair previous = null;
    Pair last = args;
    Pair next;
    while(! (next = ((Pair) last.getRest())).isEmpty()) {
      if (Scheme.isInterruptable()) Scheme.interruptCheck();
      previous = last;
      last = next;
    }
    if (previous == null) args = ((Pair) args.getFirst());
    else previous.rest = ((Pair) (last.first));	// KRA 27MAY04: 
    return p.apply(args);
  }
}
