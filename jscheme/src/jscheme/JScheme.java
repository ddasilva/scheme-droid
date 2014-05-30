package jscheme;

import java.io.PrintWriter;

import jsint.Evaluator;
import jsint.InputPort;
import jsint.Pair;
import jsint.Scheme;
import jsint.Symbol;
import jsint.U;

/**
   JScheme - a simple interface to JScheme.
   <p> This class provides methods to perform common Scheme operations from
   java.  Each instance of <code>JScheme</code> is an isolated Scheme environment.

   <p>For example, you can:
   <ul>
    <li> Create a new Scheme environment:
    <pre>
     JScheme js = new JScheme();
    </pre>
    <li> Initialize an application by loading Scheme expressions from a file:
    <pre>
      js.load(new java.io.File("app.init"));
    </pre>
    <li> Run the Scheme procedure
    <tt>(describe)</tt> to describe the current object like this:
    <pre>
    js.call("describe", this);
    </pre>

    If you need to call a procedure with more than twenty arguments
    use apply().

    <li>Evalute a string as an expression:
    <pre> String query = "(+ (expt (Math.sin 2.0) 2) (expt (Math.cos 2.0) 2))";
    System.out.println(query + " = " + js.eval(query));
    </pre> </ul>
   
   <p> Unit test:
   <pre>
   (import "jscheme.JScheme")
   (load "elf/util.scm")
   (define js (JScheme.forCurrentEvaluator))
   (assert (equal? (+ 2 3) (.eval js '(+ 2 3))))
   (assert (= (+ 2 3) (.eval js "(+ 2 3)")))
   (assert (= (+ 2 3) (.call js "+" 2 3)))
   (assert (= (+ 2 3) (.call js + 2 3)))
   (assert (= (+ 2 3) (.apply js "+" (JScheme.list 2 3))))
   (.load js "(define (f x) (+ x (g x))) (define (g x) (* x 3))")
   (assert (= (f 3) 12))
   </pre>

 **/
public class JScheme implements java.io.Serializable {

  private Evaluator evaluator;

  /**
   * Creates a new, isolated Scheme environment.
   */
  public JScheme() {
    evaluator = new Evaluator();
  }

  /**
   * Creates a Scheme environment that shares an evaluation enironment.
   * Top-level bindings will be shared.
   */
  public JScheme(Evaluator e) {
    evaluator = e;
  }

  /**
   * Returns the Scheme environment that is currently executing Scheme.  Only
   * call this from Scheme (or from Java called from Scheme).
   */
  public static JScheme forCurrentEvaluator() {
    return new JScheme(Scheme.currentEvaluator()); }

  /**
   * Returns this Scheme environment's evaluator.
   */
  public Evaluator getEvaluator() { return evaluator; }

  /**
   * Enters this instance's evaluator.
   */
  private void enter() {
    Scheme.pushEvaluator((Evaluator)evaluator); }

  /**
   * Exits an evaluator.  This must be called for every call to
   * <code>enter</code>.
   */
  private void exit() {
    Scheme.popEvaluator(); }

  /** Does the Symbol named s have a global value? **/
  public boolean isDefined(String s) {
    enter(); try { return Symbol.intern(s).isDefined();}
    finally { exit(); }}

  /** Get the value of the global variable named s. **/
  public Object getGlobalValue(String s) {
    enter(); try {
      return Symbol.intern(s).getGlobalValue(); }
    finally { exit(); } }

  /** Set the value of the global variable named s to v.  **/
  public void setGlobalValue(String s, Object v) {
    enter(); try {
      Symbol.intern(s).setGlobalValue(v); }
    finally { exit(); } }

  /** Returns the global procedure named s. **/
  public SchemeProcedure getGlobalSchemeProcedure(String s) {
    enter(); try {
      return U.toProc(getGlobalValue(s)); }
    finally { exit(); } }

  /** Load Scheme expressions from a  Reader, or String. **/
  public Object load(java.io.Reader in) {
    enter(); try {
      return Scheme.load(new InputPort(in)); }
    finally { exit(); } }

  public Object load(String in) {
    return load(new java.io.StringReader(in)); }

  /** Eval or load a string.
      This is useful for handling command line arguments.
      If it starts with "(", it is evaled.
      If it doesn't start with "-", it is loaded.
   **/
  public void evalOrLoad(String it) {
    if (it.startsWith("("))
      load(new java.io.StringReader(it));
    else if (! it.startsWith("-")) {
      enter(); try {
        Scheme.load(it); }
      finally { exit(); } } }

  /** Call a procedure with 0 to 20 arguments **/
  public Object call(SchemeProcedure p) {
    enter(); try {
      return p.apply(list()); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1) {
    enter(); try {
      return p.apply(list(a1)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2) {
    enter(); try {
      return p.apply(list(a1, a2)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3) {
    enter(); try {
      return p.apply(list(a1, a2, a3)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)); }
    finally { exit(); } }
  public Object call(SchemeProcedure p, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
    enter(); try {
      return p.apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)); }
    finally { exit(); } }

  /** Apply a procedure to a list of arguments. **/
  public Object apply(SchemeProcedure p, SchemePair as) {
    enter(); try {
      return p.apply(as); }
    finally { exit(); } }

  public Object apply(SchemeProcedure p, Object[] args) {
    enter(); try {
      return p.apply(args); }
    finally { exit(); } }

  /** Call a procedure named p with from 0 to 20 arguments. **/
  public Object call(String p) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list()); }
    finally { exit(); } }
  public Object call(String p,Object a1) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)); }
    finally { exit(); } }
  public Object call(String p,Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)); }
    finally { exit(); } }

  /** Apply a procedure named p to a list of arguments. **/
  public Object apply(String p, SchemePair as) {
    enter(); try {
      return getGlobalSchemeProcedure(p).apply(as); }
    finally { exit(); } }

  /** Evaluate the contents of a string as a Scheme expression.  **/
  public Object eval(String s) {
    Object it = read(s);
    return it == InputPort.EOF ? it : eval(it); }
  
  /** Evaluate an expression Object **/
  public Object eval(Object it) {
    enter(); try {
      return Scheme.evalToplevel(it,jsint.Scheme.getInteractionEnvironment()); }
    finally { exit(); } }

  /** Read an expression from a String. **/
  public Object read(String s) {
    enter(); try {
      return (new InputPort(new java.io.StringReader(s))).read(); }
    finally { exit(); } }

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

  public void write(Object x, PrintWriter port, boolean quoted) {
    enter(); try {
      U.write(x, port, quoted); }
    finally { exit(); } }
  public void write(Object x, PrintWriter port) {
    enter(); try {
      U.write(x, port, true); }
    finally { exit(); } }
  public void display(Object x, PrintWriter port) {
    enter(); try {
      U.write(x, port, false); }
    finally { exit(); } }

  public void readEvalPrintLoop() {
    enter(); try {
      Scheme.readEvalWriteLoop(">");
    } finally { exit(); }
  }


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
