package jsint;

/** 
 * Abstract superclass of Procedures.  Procedures of no arguments,
 * called "thunks", implement the Runnable interface.
 *
 * To invoke a Procedure from Java, use apply(Pair).
 *
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/
public abstract class Procedure implements Runnable, java.io.Serializable, jscheme.SchemeProcedure {

  public String name = "??";
  public int minArgs = 0;
  public int maxArgs = Integer.MAX_VALUE;
  
  // bogus.
  public Procedure() {; }
  public Procedure(int minArgs, int maxArgs) {
    this.minArgs = minArgs ; this.maxArgs = maxArgs; }
  // end bogus.

  public String getName() { return this.name; }
  
  /** If the procedure has not yet been named, name it. 
   * The idea is if I do <tt>(define (id x) x)</tt> and then
   * <tt>(define default-proc id)</tt>, then the procedure is called "id". **/
  public Object setName(Object name) {
    if (this.name == "??") this.name = name.toString();
    return name;
  }

  /** How many parameters does this procedure have.  This is different than the
   * number of arguments it expects.  For example, <tt>list</tt> expects any
   * number of arguments, but it has only one parameter. **/
  public int nParms() {
    return (minArgs == maxArgs) ? minArgs : minArgs+1;
  }
  
  protected String toStringArgs() {
    if (minArgs == maxArgs) return "[" + minArgs + "]";
    else return "[" + minArgs + "," + 
	   ((maxArgs == Integer.MAX_VALUE) ? "n" : String.valueOf(maxArgs)) + "]"; 
  }

  public String toString() { 
    return "{" + this.getClass().getName() + " " + name + toStringArgs() + "}"; }

  public void run() {
    if (this.minArgs == 0) this.apply(Pair.EMPTY);
    else E.error("This procedure can't be run()", this);
  }

  /** Apply the procedure to an argument list, which is represented as a
   * parameter-oriented array.  That is, if the procedure p has the parameter
   * list <tt>(x y . z)</tt> and the call is <tt>(p 1 2 3 4)</tt> then  
   * <tt>args</tt> will be the array <tt>{1, 2, (3 4)}</tt>. **/
  public abstract Object apply(Object[] args);
  
  public Object apply(jscheme.SchemePair args) {
    return apply(this.makeArgArray((Pair) args));
  }

  /** Convert an argument list into an argument array, and call <tt>apply</tt>
   * on that. **/
  public Object apply(Pair args) {
    return apply(this.makeArgArray(args));
  }
   
  /** Like tryCatch, but returns wrapped exception. */
  public static Object catching(Procedure E, Procedure F) {
    try {
      return E.apply(Pair.EMPTY);
    }catch(Throwable e) {
	return F.apply(new Pair(e,Pair.EMPTY));}}


  /** provide scheme access to the "try/catch" expression of Java */
  public static Object tryCatch(Object E, Object F) {
    try {
      return ((Procedure) E).apply(Pair.EMPTY);
    }catch(Throwable e) {
	return ((Procedure) F).apply(new Pair(stripExceptionWrapper(e),Pair.EMPTY));}}

    /* this strips off the wrappers so that the tryCatch receives the
       underlying exception */
    public static Object stripExceptionWrapper(Object e) {
	if (e instanceof BacktraceException)
            return stripExceptionWrapper(((BacktraceException) e).getBaseException());
	else if (e instanceof JschemeThrowable)
            return stripExceptionWrapper(((JschemeThrowable) e).contents);
        else
            return e;
    }
  


  /** Provide scheme access to finally -  unwind-protect. **/
  public static Object tryFinally(Object e, Object f) {
    try     { return ((Procedure) e).apply(Pair.EMPTY); }
    finally { ((Procedure) f).apply(Pair.EMPTY); }
  }

  /** provide scheme access to the exception throwing */
  public static Object throwRuntimeException(RuntimeException E) throws RuntimeException { 
    throw(E);
  }

  public static Object throwObject(Object e) throws Throwable { 
    if (e instanceof Throwable) throw((Throwable)e);
    else return E.error("can't throw object "+e);
  }

  public static Object synchronize(Object x, Procedure p) {
    synchronized(x) { return p.apply(new Pair(x, Pair.EMPTY)); }
  }

  /** This is called during function application in Evaluator.execute. **/
  /** Take the code that represents a call, evaluate arguments in the calling
   * lexical environment and put results into an argument array. Note that
   * <tt>code[0]</tt> is the procedure, not an argument. For example,
   * <tt>(p 1 2 3 (+ 2 2))</tt> turns into the code <tt>{p, 1, 2, 3, {+, 2, 2}}</tt>, 
   * and if <tt>p</tt> has the parameter list <tt>(x y . z)</tt> then 
   * <tt>p.makeArgArray(code, lexenv)</tt> would return <tt>{1, 2, (3 4)}</tt>. **/

  public Object[] makeArgArray(Object[] code,
                               Evaluator eval,
                               LexicalEnvironment lexenv) {
    // KRA 07MAR02: Inlined to reduce stack space.
    // return Scheme.makeArgArray(this, code, lexenv);
    Procedure p = this;
        Object[] argArray = new Object[p.nParms()];
    int pminargs = p.minArgs;
    int nargs = code.length-1;
    if (nargs < pminargs) E.error("\nToo few arguments to procedure "+p.name+" expected at least "+pminargs
				 +", but found "+ nargs+" arguments:\n***************\n    "+U.stringify(code)+"\n************\n");
    if (nargs > p.maxArgs) E.error("\nToo many arguments to procedure "+p.name+" expected at most "+p.maxArgs
				 +", but found "+ nargs+" arguments:\n***************\n    "+U.stringify(code)+"\n************\n");

    // Fill in the required parameters
    for (int i = 0; i < pminargs; i++) {
      argArray[i] = eval.execute(code[i+1], lexenv);
    }
    // Add the remaining parameter (if there is one)
    if (p.maxArgs > pminargs) {
      if (p.maxArgs == pminargs+1) // single optional argument for primitive.
	argArray[pminargs] = (code.length > p.maxArgs) 
	  ? eval.execute(code[pminargs+1], lexenv) : U.MISSING;
      else { // "rest" argument
	Object tail = Pair.EMPTY;
        for (int i = nargs; i > pminargs; i--)
          tail = new Pair(eval.execute(code[i], lexenv), tail);
	argArray[pminargs] = tail;

      }
    }
    return argArray;
  }

   /** Convert a list of arguments into an array expected by the procedure.
    * (See <tt>apply</tt> for an explanation of what is expected.) **/
   public Object[] makeArgArray(Pair args) {
     Procedure p = this;
     Object[] argArray = new Object[p.nParms()];
     int nargs = args.length();
     if (nargs < p.minArgs) E.error("\nToo few arguments to procedure "+p.name+" expected at least "+p.minArgs
 				 +", but found "+ nargs+" arguments:\n***************\n    "+U.stringify(args)+"\n************\n");
     if (nargs > p.maxArgs) E.error("\nToo many arguments to procedure "+p.name+" expected at most "+p.maxArgs
 				 +", but found "+ nargs+" arguments:\n***************\n    "+U.stringify(args)+"\n************\n");
     // Fill in the required parameters
     for (int i = 0; i < p.minArgs; i++, args = U.toList(args.rest)) {
       argArray[i] = args.first;
     }
     // Add the remaining parameter (if there is one)
     if (p.maxArgs > p.minArgs) {
       if (p.maxArgs == p.minArgs+1) // single optional argument
 	argArray[p.minArgs] = (U.isPair(args)) ? args.first : U.MISSING;
       else // "rest" argument
 	argArray[p.minArgs] = args;
     }
     return argArray;
   }
}
