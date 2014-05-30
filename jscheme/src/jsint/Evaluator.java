package jsint;
import java.io.*;
import java.util.HashMap;
/** This class represents a Scheme interpreter.
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 **/

public class Evaluator implements java.io.Serializable {
  /** Should REP Loop exit? **/
  private boolean exit = false;
  public boolean setExit(boolean exit) {
    return this.exit = exit;
  }

  private transient InputPort   input  = new InputPort(System.in);
  public void setInput(InputPort ip) { this.input = ip; }
  public InputPort getInput() { return this.input; }

  private transient PrintWriter output = new PrintWriter(System.out, true);
  public PrintWriter getOutput() { return this.output; }
  public void setOutput(PrintWriter w) {this.output = w;}

  private transient PrintWriter error  = new PrintWriter(System.err, true);
  public PrintWriter getError() { return this.error; }
  public void setError(PrintWriter w) { this.error = w; }

  /** Is execution interruptable. **/
  public boolean INTERRUPTABLE = false;
  /** Interrupt execution on thread <tt>t</tt>. **/
  public void interrupt (Thread t) {
    INTERRUPTABLE = true;
    t.interrupt();
  }
  /** Maybe interrupt this thread of execution. **/
  public void interruptCheck() {
    if (INTERRUPTABLE && Thread.currentThread().interrupted()) {
      INTERRUPTABLE = false;
      throw new JschemeThrowable("Execution was interrupted.");
    }
  }
  // The default environment for doing work.
  // We have to define and initialize this before the static block
  // that loads the primitives.
  public DynamicEnvironment interactionEnvironment
      = null;
      //    = new DynamicEnvironment();

  // The R5RS "null-environment".
  public static DynamicEnvironment NULL_ENVIRONMENT
    = new DynamicEnvironment();

  // The environment at system startup.  Any loaded environments are
  // created based off of this.
  public DynamicEnvironment INITIAL_ENVIRONMENT = null;

  public DynamicEnvironment getInteractionEnvironment() {
    return interactionEnvironment;
  }

  public static DynamicEnvironment getNullEnvironment() {
    return NULL_ENVIRONMENT;
  }

  public DynamicEnvironment getInitialEnvironment() {
    return INITIAL_ENVIRONMENT;
  }

  private static HashMap environmentCache = new HashMap();
  private static DynamicEnvironment BASE_ENVIRONMENT = null;

  static {
    NULL_ENVIRONMENT.lockDown();
  }

  public Evaluator() {
    Scheme.pushEvaluator(this);
    try {
      if (BASE_ENVIRONMENT==null) {
	interactionEnvironment = new DynamicEnvironment();
	jsint.Primitive.loadPrimitives();
	BASE_ENVIRONMENT = new DynamicEnvironment
	  (this.interactionEnvironment);
	BASE_ENVIRONMENT.lockDown();
      }
      else {
	interactionEnvironment = new DynamicEnvironment(BASE_ENVIRONMENT);
      }
      INITIAL_ENVIRONMENT = new DynamicEnvironment(interactionEnvironment);
      INITIAL_ENVIRONMENT.lockDown();
    } finally {
      Scheme.popEvaluator();
    }
  }

  public Evaluator(DynamicEnvironment env) {
      interactionEnvironment = new DynamicEnvironment(env);
      INITIAL_ENVIRONMENT = new DynamicEnvironment(interactionEnvironment);
      INITIAL_ENVIRONMENT.lockDown();
  }

  public void runJscheme() {
    if (exit) return;
    showVersion();
    readEvalWriteLoop("> ");
  }

  /**
   *  If true, results of REPL evaluations are named (e.g. $3) for
   *  future reference.
   */
  private boolean nameResults = true;

  public void enableNamedResults(boolean enabled) {
    nameResults = enabled;
  }

  /** Prompt, read, eval, and write the result.
   * Also sets up a catch for any RuntimeExceptions encountered.
   * Returns true on EOF, false if (exit) was evaluated. **/
  public boolean readEvalWriteLoop(String prompt) {
    int count = 0;
    while(!exit) {
      try {
	output.print(prompt); output.flush();
      	Object x = input.read();
	if (x == InputPort.EOF) return true; // EOF
	Object result = eval(x);
	if(nameResults) {
	  String name = "$" + (++count);
	  output.print(name+" = ");
	  interactionEnvironment.setValue(Symbol.intern(name), result);
	}
	U.write(result, output, true);
	output.write("\n");
	output.println(); output.flush();
      } catch (Throwable e) {
	e.printStackTrace(error);
	error.flush();
      }
    }
    return false; // (exit) called.
  }

    /**
     * load the current object (file or class) into a new Evaluator
     * and return the resulting Evaluator's DynamicEnvironment. 
     */
  public DynamicEnvironment loadEnvironment(Object x) {

    Object cacheKey = x;
    Object cached = environmentCache.get(cacheKey);

    // for filenames, use the CanonicalFile name as the hashkey, for
    // others use the object itself
    try {
      if (x instanceof String) {
        cacheKey = (new java.io.File((String) x)).getCanonicalFile()
          .toString().intern();
        cached = environmentCache.get(cacheKey);
      }
    }catch(Exception e)	{ }


    if (cached != null) return (DynamicEnvironment) cached;
    else {
      Evaluator evaluator = new Evaluator();
      DynamicEnvironment env = evaluator.interactionEnvironment;
      Scheme.pushEvaluator(evaluator);
      evaluator.load(x);
      env = evaluator.interactionEnvironment; // the evaluator might change its interactionEnvironment
      Scheme.popEvaluator();
      env.lockDown();
      environmentCache.put(cacheKey, env);
      return(env);
    }
  }

    public Boolean environmentImport(Object x, Object prefix) {
      return environmentImport(x,prefix, false, null);
  }

  public Boolean environmentImport(Object x, Object prefix,
                                   boolean macrosFlag,
                                   jsint.Symbol[] procnames) {
    synchronized (interactionEnvironment) {
      DynamicEnvironment env = loadEnvironment(x);
      if (prefix instanceof String) 
        if (macrosFlag && (((String)prefix).length() > 0)) {
          E.error("(environment-import): macros cannot have a prefix"+
                  prefix);
          return Boolean.FALSE;
        } else {
          interactionEnvironment.importBindings(env, (String) prefix,
                                                macrosFlag, procnames);
          return Boolean.TRUE;
        } else if ((prefix == U.MISSING) ||
                   ((prefix instanceof Boolean) &&
                    ((Boolean) prefix) == Boolean.FALSE)) {
          interactionEnvironment.importBindings(env, null,
                                                macrosFlag, procnames);
          return Boolean.TRUE;
        } else {
          E.error("(environment-import): prefix is not string or #f: "
                  +prefix);
          return Boolean.FALSE;
        }
    }
  }

  public Boolean languageImport(Object x) {
    synchronized (interactionEnvironment) {
      DynamicEnvironment env = loadEnvironment(x);
      interactionEnvironment.importBindings(env, null, true);
      return Boolean.TRUE;
    }
  }

  /** Eval all the expressions in a file. Calls load(InputPort). **/
  public Object load(Object fileName) {
    String name = fileName.toString();
    InputPort iport = Scheme.open(name);
    if (iport == null) return E.warn("(load) can't open \"" + fileName + "\"");
    else return load(iport);
  }
  

  /** Eval all the expressions coming from an InputPort, putting them
      in the interactionEnvironment.  The interactionEnvironment might
      have been rebound to a module environment, but we don't care
      about that.  Returns TRUE on EOF, FALSE if (exit) was evaluated.
  **/
  public Object load(InputPort in) {
    while(!exit) {
      try {
	Object x = in.read();
	if (x == InputPort.EOF) return U.TRUE;
	else evalToplevel(x, interactionEnvironment);
      } catch(Exception e) {
        E.warn("Error during load (lineno "+in.getLineNumber()+"): ", e);
        e.printStackTrace(error);
      }
    }
    return U.FALSE; // (exit)
  }

  /** evalToplevel evaluates each element of a BEGIN.  This is so
      macros can be defined and then used.  Also toplevel macros can
      expand into begin.
  **/
  public Object evalToplevel(Object x, DynamicEnvironment env) {
    if (U.isPair(x)) {
      Object mx = Macro.expand((Pair) x);
      if (x != mx) return evalToplevel(mx, env);
      else if (U.first(x) == Symbol.BEGIN) {
	Object xs = U.rest(x);
	Object result = null;
	while (U.isPair(xs)) {
	  result = eval(U.first(xs), env);
	  xs = U.rest(xs);
	}
	return result;
      } else return eval(x, env);
    } else return eval(x, env);
  }

  //////////////// Evaluation ////////////////

  /** Evaluate an s-expression in the global environment. **/
  public Object eval(Object x) {
    return eval(x, interactionEnvironment);
  }

  /** Evaluate an s-expression in a lexical environment. First analyze
   * it.
   **/
  public Object eval(Object x, Object env) {
    DynamicEnvironment dynamicEnv = ((env == U.MISSING)
                                     ? interactionEnvironment
                                     : (DynamicEnvironment) env);
    Object analyzedCode = analyze(x,dynamicEnv,LexicalEnvironment.NULLENV);
    return execute(analyzedCode,LexicalEnvironment.NULLENV);
  }

  /** Analyze (or preprocess or precompile) an expression into "code".
   * The code returned is either a Symbol, a LocalVariable, a Closure, or
   * an array whose first element is either one of (quote if or begin set!)
   * or is code evaluating to a procedure. **/
  public Object analyze(Object x, DynamicEnvironment dynamicEnv,
                        LexicalEnvironment lexenv) {
    if (x instanceof Symbol) {
      LocalVariable localvar = lexenv.lookup((Symbol)x);         // Convert symbol to variable
      if (localvar == null) return dynamicEnv.intern((Symbol) x);
      else return localvar;
    }
    else if (!U.isPair(x))
      return new Object[] {Symbol.QUOTE, x};   // Convert 42 to #(quote 42)
    else {
      Object f = U.first(x), xm;
      int len = ((Pair)x).length();
      if (Symbol.LAMBDA == f && len >= 3) { // Handle (lambda ...)
	Object args = U.second(x);
	LexicalEnvironment lexenv2 = new LexicalEnvironment (args,
                                                             null,
                                                             lexenv);
	return new Closure(args,
			   analyze(Scheme.toBody(U.rest(U.rest(x))),
                                   dynamicEnv,
                                   lexenv2)
			   , lexenv);
      }
      else if (Symbol.MACRO == f && len >= 3) {	// Handle (macro ...)
	Object args = U.second(x);
	LexicalEnvironment lexenv2 = new LexicalEnvironment(args,
                                                            null,
                                                            lexenv);
	return new Macro(args,
			 analyze(Scheme.toBody(U.rest(U.rest(x))),
                                 dynamicEnv,
                                 lexenv2),
			 lexenv);
      }
      else if (2 == len && Symbol.BEGIN == f)
	return analyze(U.second(x), dynamicEnv, lexenv);     // Convert (begin x) to x
      else if (x != (xm = Macro.expand((Pair)x)))
	return analyze(xm, dynamicEnv, lexenv);              // Analyze macroexpansion
      else if (Symbol.OR == f && len == 1)
	return new Object[] {Symbol.QUOTE, U.FALSE};// (or) => '#f
      else if (Symbol.IF == f && len == 3)       // (if a b) => (if a b #f)
	return analyze(U.append(U.list(x, U.list(U.FALSE))),
                       dynamicEnv,
                       lexenv);
      else if (Symbol.QUOTE == f && len == 2)
	return new Object[] { Symbol.QUOTE, U.second(x)};
      else {
	// Complain if there is was syntax error.
	checkLength(f, len, x);
	// Compile into a special form, or an application.
	Object[] xv = U.listToVector(x);         // Convert list to vector
	if (! isSpecial(f)) xv[0] = analyze(xv[0], dynamicEnv, lexenv);
	for (int i = 1; i < xv.length; i++) {
	  xv[i] = analyze(xv[i], dynamicEnv, lexenv);        // Convert each element
	}
	return xv;
      }
    }
  }

  /** Evaluate analyzed code in a lexical environment. Don't pass this
   * a raw s-expression; you need to analyze the s-expression
   * first.
   **/
   public Object execute(Object x, LexicalEnvironment lexenv) {
    // The purpose of the while loop is to allow tail recursion.
    // The idea is that in a tail recursive position, we do "x = ..."
    // and loop, rather than doing "return execute(...)".
    do {
      if (!(x instanceof Object[])) {
	if (x instanceof DynamicVariable)
	  return ((DynamicVariable)x).getDynamicValue();
	else if (x instanceof LocalVariable)
	  return lexenv.get((LocalVariable)x);
	else return ((Closure)x).copy(lexenv);
      } else {
	Object[] xv = (Object[])x;
	Object f = xv[0];
	if (f == Symbol.QUOTE) return xv[1];
        else if (f == Symbol.IF)
	  x = (U.to_bool(execute(xv[1], lexenv))) ? xv[2] : xv[3];
	else if (f == Symbol.BEGIN) x = executeButLast(xv, lexenv);
	else if (f == Symbol.OR) {
	  int xvlm1 = xv.length-1;
	  for (int i = 1; i < xvlm1; i++) {
	    Object result = execute(xv[i], lexenv);
	    if (U.toBool(result) != U.FALSE) return result;
	  }
	  // Replace x with the final one
	  x = xv[xvlm1];
	} else if (f == Symbol.SET && xv[1] instanceof DynamicVariable)
	  return ((DynamicVariable)xv[1]).setDynamicValue
	    ( execute(xv[2], lexenv));
	else if (f == Symbol.SET && xv[1] instanceof LocalVariable)
	  return lexenv.set((LocalVariable)xv[1], execute(xv[2], lexenv));
	else {		// Function application.
	  if (INTERRUPTABLE) interruptCheck();
	  try {
	    f = executef(f, lexenv);
	    if (f instanceof Closure) {
	      Closure c = (Closure)f;
	      x = c.body;
	      lexenv = new LexicalEnvironment
		(c.parms, c.makeArgArray(xv, this, lexenv), c.lexenv);
	    } else if (f instanceof Generic) {
	      Generic g = ((Generic) f);
	      Object[] args = g.makeArgArray(xv, this, lexenv);
	      Closure c = g.findMethod(args);
	      x = c.body;
	      lexenv = new LexicalEnvironment(c.parms, args, c.lexenv);
	    } else {		// OTHER PROCEDURE CALL
	      Procedure p = U.toProc(f);
	      return p.apply(p.makeArgArray(xv, this, lexenv));
	    }
	    // Continuations, and (messageless) JschemeThrowables pass through
	  } catch (ContinuationException ce) { throw ce;
          } catch (RuntimeException e) {
            if ((e instanceof JschemeThrowable) && (e.getMessage() == null))
	      throw e;
	    else throw new BacktraceException(e, xv, lexenv);
	  }
	}
      }
    }while (true);
  }

  /** Specialized execute() where x is the function of an application. **/
  private Object executef(Object x, LexicalEnvironment lexenv) {
    if (x instanceof DynamicVariable)
      return ((DynamicVariable)x).getDynamicValue();
    else if (x instanceof LocalVariable) return lexenv.get((LocalVariable)x);
     else if (x instanceof Closure) return ((Closure)x).copy(lexenv);
    else return execute(x, lexenv);
  }

  private static boolean isSpecial (Object f) {
    return f == Symbol.SET || f == Symbol.IF || f == Symbol.BEGIN ||
      f == Symbol.OR || f == Symbol.QUOTE; }

  private static void checkLength(Object f, int len, Object x) {
    if ((f == Symbol.LAMBDA && len < 3) ||
	(f == Symbol.MACRO  && len < 3) ||
	(f == Symbol.SET    && len != 3) ||
        (f == Symbol.IF     && len != 4) ||
        (f == Symbol.BEGIN  && len <= 2) ||
        (f == Symbol.OR     && len <  2) ||
        (f == Symbol.QUOTE  && len != 2))
      E.warn("wrong number of arguments for syntax:", x);
  }
  /** Evaluate **/
  private Object executeButLast(Object[] xv, LexicalEnvironment lexenv) {
    for (int i = 1; i < xv.length-1; i++) {
      execute(xv[i], lexenv);
    }
    return xv[xv.length-1];
  }

  private void showVersion() {
    error.println(getVersion());
  }

  private String getVersion() {
    String defaultmsg = "Jscheme http://jscheme.sourceforge.net";
    String name = "jsint/version.txt";
    ClassLoader loader = Import.getClassLoader();
    InputStream stream = (loader == null) ?
      ClassLoader.getSystemResourceAsStream(name) :
      loader.getResourceAsStream(name);
    if (stream == null) return defaultmsg;
    else {
      BufferedReader r = new BufferedReader(new InputStreamReader(stream));
      try {return r.readLine();}
      catch (IOException e) { return defaultmsg; }
    }
  }
}
