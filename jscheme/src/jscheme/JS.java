package jscheme;

import java.io.PrintWriter;

import jsint.Evaluator;
import jsint.InputPort;
import jsint.Pair;
import jsint.Scheme;
import jsint.Symbol;
import jsint.U;

/**
   JS - a simple interface to JScheme.
   <p> This class provides static methods to perform common Scheme
   operations from java. Using a single JScheme instance, js.

 **/
public class JS {

    public JS() {;}

  public static final JScheme js = new JScheme (firstEvaluator());

  /** This is the default evaluator used by all threads that don't
      explictily push/pop their own. **/
  private static Evaluator firstEvaluator() {
    return  new Evaluator();
  }

  /** Does the Symbol named s have a global value? **/
  public static boolean isDefined(String s) { return js.isDefined(s); }

  /** Get the value of the global variable named s. **/
  public static Object getGlobalValue(String s) {
    return js.getGlobalValue(s); }

  /** Set the value of the global variable named s to v.  **/
  public static void setGlobalValue(String s, Object v) {
    js.setGlobalValue(s,v); }

  /** Returns the global procedure named s. **/
  public static SchemeProcedure getGlobalSchemeProcedure(String s) {
    return js.getGlobalSchemeProcedure(s);}

  /** Load Scheme expressions from a  Reader, or String. **/
  public static Object load(java.io.Reader in) {
    return js.load(in);}

  public static Object load(String in) {
    return js.load(in);}

  /** Eval or load a string.
      This is useful for handling command line arguments.
      If it starts with "(", it is evaled.
      If it doesn't start with "-", it is loaded.
   **/
  public static void evalOrLoad(String it) {
      js.evalOrLoad(it);}

  /** Call a procedure with 0 to 20 arguments **/
  public static Object call(SchemeProcedure p) {
      return js.call(p);}
  public static Object call(SchemeProcedure p, Object a1) {
    return js.call(p,a1); }
  public static Object call(SchemeProcedure p, Object a1, Object a2) {
    return js.call(p, a1, a2); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3) {
    return js.call(p, a1, a2, a3); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4) {
    return js.call(p, a1, a2, a3, a4); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5) {
    return js.call(p, a1, a2, a3, a4, a5); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
    return js.call(p, a1, a2, a3, a4, a5, a6); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); }
  public static Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); }

  /** Apply a procedure to a list of arguments. **/
  public static Object apply(SchemeProcedure p, SchemePair as) {
    return js.apply(p,as); }

  /** Call a procedure named p with from 0 to 20 arguments. **/
  public static Object call(String p) {
    return js.call(p); }
  public static Object call(String p,Object a1) {
    return js.call(p, a1); }
  public static Object call(String p,Object a1, Object a2) {
    return js.call(p, a1, a2); }
  public static Object call(String p,Object a1, Object a2, Object a3) {
    return js.call(p, a1, a2, a3); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4) {
    return js.call(p, a1, a2, a3, a4); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5) {
    return js.call(p, a1, a2, a3, a4, a5); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
    return js.call(p, a1, a2, a3, a4, a5, a6); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19); }
  public static Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
    return js.call(p, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20); }

  /** Apply a procedure named p to a list of arguments. **/
  public static Object apply(String p, SchemePair as) {
      return js.apply(p,as);}

  /** Evaluate the contents of a string as a Scheme expression.  **/
  public static Object eval(String s) {
      return js.eval(s);}

  /** Evaluate an expression Object **/
  public static Object eval(Object it) {
      return js.eval(it);}

  /** Read an expression from a String. **/
  public static Object read(String s) {
      return js.read(s);}

  /** Lists of length 0 to 20. **/
  public static SchemePair list() { return Pair.EMPTY; }
  public static SchemePair list(Object a0) {
    return new Pair(a0, list()); }
  public static SchemePair list(Object a0, Object a1) {
    return new Pair(a0, list(a1)); }
  public static SchemePair list(Object a0, Object a1, Object a2) {
    return new Pair(a0, list(a1, a2)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3) {
    return new Pair(a0, list(a1, a2, a3)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4) {
    return new Pair(a0, list(a1, a2, a3, a4)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5) {
    return new Pair(a0, list(a1, a2, a3, a4, a5)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)); }
  public static SchemePair list(Object a0, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
    return new Pair(a0, list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)); }

  public static void write(Object x, PrintWriter port, boolean quoted) {
    js.write(x, port, quoted); }
  public static void write(Object x, PrintWriter port) {
    js.write(x, port, true); }
  public static void display(Object x, PrintWriter port) {
    js.write(x, port, false); }

  /** Convert from an Object to a primitive type. **/
  public static boolean booleanValue(Object o) { return o != Boolean.FALSE; }
  public static byte byteValue(Object o) { return ((Number) o).byteValue(); }
  public static char charValue(Object o) {
    return (o instanceof Number) ? ((char) ((Number) o).shortValue()) :
      ((Character) o).charValue(); }
  public static short shortValue(Object o) { return ((Number) o).shortValue(); }
  public static int     intValue(Object o) { return ((Number) o).intValue(); }
  public static long    longValue(Object o) { return ((Number) o).longValue(); }
  public static float   floatValue(Object o) { return ((Number) o).floatValue(); }
  public static double  doubleValue(Object o) { return ((Number) o).doubleValue(); }

  /** Convert from primitive type to Object. **/
  public static Boolean toObject(boolean x) { return x ? Boolean.TRUE :
    Boolean.FALSE; }
  public static Object toObject(byte x) { return new Byte(x); }
  public static Object toObject(char x) { return new Character(x); }
  public static Object toObject(short x) { return new Short(x); }
  public static Object toObject(int x) { return U.toNum(x); }
  public static Object toObject(long x) { return new Long(x); }
  public static Object toObject(float x) { return new Float(x); }
  public static Object toObject(double x) { return new Double(x); }
  public static Object toObject(Object x) { return x; }	// Completeness.
}    
