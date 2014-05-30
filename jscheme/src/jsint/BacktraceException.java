package jsint;

/**  
 * A jsint.BacktraceException is used to capture and report on
 * uncaught Exceptions thrown in a Jscheme program.
 * @author Ken R. Anderson, Copyright 2000, kanderso@bbn.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class BacktraceException extends RuntimeException {
  public static boolean printJavaTrace = false;
  private Throwable exception;
  private Object[] args;
  private LexicalEnvironment lexenv;

  public BacktraceException(Throwable e, Object[] args) {
    this(e,args,jsint.LexicalEnvironment.NULLENV);
  }

  public BacktraceException(Throwable e, Object[] args, LexicalEnvironment lexenv) {
    // KRA 25DEC01: super suggestion by Adrian Boyko <adrianboyko@hotmail.com> 
    super(e.getMessage());
    this.exception = e;
    this.args = args;
    this.lexenv = lexenv;
  }

  /** Clever performance trick i found in
      http://docs.msdnaa.net/ark_new/Webfiles/WhitePapers/Babel01/bab12.pdf
  **/
  public Throwable fillInStackTrace() {
    return this;
  }
  public Throwable getBaseException() {
    Throwable t = this.exception;
    while (t instanceof BacktraceException) 
      t=((BacktraceException) t).exception;
    return t;
  }

  public void printStackTrace(java.io.PrintStream s) {
    this.printStackTrace(new java.io.PrintWriter(s));
  }

  public void printStackTrace() {
    this.printStackTrace(Scheme.currentEvaluator().getError());
  }

  private static final Symbol backtraceBody = Symbol.intern("backtraceBody");

  /** Try once to load code for a nicer backtrace.  If you fail use
      the older one. **/
  private static boolean triedOnce = false;
  public static boolean checkBacktrace() {
    try {
//       if (jscheme.JS.isDefined("backtraceBody")) return true;
      if (Scheme.currentEvaluator().getInteractionEnvironment().
	isDefined(backtraceBody)) return true;
      if (triedOnce) return false;
      triedOnce = true;
      Scheme.load("elf/basic.scm");
      return true;
    } catch (Throwable t) {
      return false;
    }
  }

  public void printStackTrace(java.io.PrintWriter s) {
    try {
      //        if (checkBacktrace())
      //    	jscheme.JS.call("backtraceBody", lexenv, args);
      //        else showargs(args, s);
      if (checkBacktrace()) {
	// was: jscheme.JS.call("backtraceBody", lexenv, args);
	Object bbProc = Scheme.currentEvaluator().getInteractionEnvironment().
	  getValue(backtraceBody);
	U.toProc(bbProc).apply(jscheme.JScheme.list(lexenv, args));
      } else {
	s.print("showargs: ");
	showargs(args, s); // args is "analyzed form": may look strange
      }
      s.println();
      lexenv.show(s);
      s.println("\n  ====================================");
      if (printJavaTrace || (this.exception instanceof BacktraceException))
	this.exception.printStackTrace(s);
      else s.println(this.exception);
      s.flush();
    } catch (Throwable e) {
      s.println("Error in BacktraceException.printStackTrace: ");
      try {ultimateException(e).printStackTrace(s);}
      catch (Throwable e2) {
	try {s.println("Error tracing the ultimate error: " +
		       ultimateException(e));}
	catch (Throwable e3) {
	  s.println("Error printing ultimate error of class: " +			     ultimateException(e).getClass());
	}}}}

   public void showargs(Object x, java.io.PrintWriter s) {
    if (x == null) s.println();
    else if (x.getClass().isArray()) {
      Object[] xv = (Object[]) x;
      if (xv.length > 0 && Symbol.QUOTE == xv[0])
	s.print(U.stringify(xv[1])+" ");
      else {
	s.print("(");
	for (int i=0; i<xv.length; i++)
	  showargs(xv[i],s);
	s.print(")");
      }
    }
    else if (x.getClass() == LocalVariable.class)
      s.print(((LocalVariable)x).name+" ");
    else
      s.print(U.stringify(x)+" ");
  }

  private static Throwable ultimateException(Throwable e) {
    return (e instanceof BacktraceException) ?
      ultimateException(((BacktraceException) e).exception) : e;
  }

}
