package jsint;

/** Environments store mappings from symbols to locations.
 * At compile time, we can lookup to see if a symbol names a location,
 * and at run time we can get or set the value in a location.
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/


public class LexicalEnvironment implements java.io.Serializable {

  /** The global environment.  All info is in the Symbols themselves. **/
  public static final LexicalEnvironment NULLENV
    = new LexicalEnvironment(Pair.EMPTY, null, null, true);

  /** The parent LexicalEnvironment: where you look if you can't find
   * a variable.
   **/
  private LexicalEnvironment parent;
  /** A list of variables in this env.  May be dotted or a lone
   * symbol.
   **/
  private Object vars;
  /** An array of values in a 1-1 correspondance with the vars. **/
  private Object[] vals;

  private boolean isNullEnv = false;

  /** Create an LexicalEnvironment with local variables and values,
   * and a parent LexicalEnvironment.
   **/
  public LexicalEnvironment(Object vars, Object[] vals,
                            LexicalEnvironment parent) {
    this(vars, vals, parent, false);
  }

  public LexicalEnvironment(Object vars, Object[] vals,
                            LexicalEnvironment parent, boolean isNullEnv) {
    this.vars = vars; this.vals = vals; this.parent = parent;
    this.isNullEnv = isNullEnv;
  }

  private Object readResolve() throws java.io.ObjectStreamException {
    return isNullEnv ? NULLENV : this;
  }

  public LocalVariable lookup(Symbol var) { return lookup(var, 0, 0, vars); }

  /** Lookup the symbol in the environment.  Return either a
   * LocalVariable a Symbol representing the global variable.
   **/
  public LocalVariable lookup(Symbol var, int up, int in, Object vars) {
    if (this == NULLENV) return null;
    else if (vars == var) return new LocalVariable(up, in, var);
    else if (!U.isPair(vars)) return parent.lookup(var, up+1, 0, parent.vars);
    else if (U.first(vars) == var) return new LocalVariable(up, in, var);
    else return lookup(var, up, in+1, U.rest(vars));
  }

  /** Get the value of the LocalVariable in this LexicalEnvironment. **/
  public Object get(LocalVariable var) {
    return this.up(var.up).vals[var.in];
  }

  /** Set the value of the LocalVariable in this LexicalEnvironment to
   * a new value.
   **/
  public Object set(LocalVariable var, Object newVal) {
    return this.up(var.up).vals[var.in] = newVal;
  }

  /** Go up a specified number of levels in the parent chain. **/
  LexicalEnvironment up(int levels) {
    return (levels == 0) ? this : this.parent.up(levels - 1);
  }

  public String toString() {
    return "{|" + toString0() + "|}";
  }
  private String toString0() {
    if (this == NULLENV) return "";
    else return U.stringify(vars) + " " + U.stringify(vals) +
	   ((parent == null) ? "" : "|" + parent.toString0());
  }

  public void show(java.io.PrintWriter s) {
    LexicalEnvironment e = this;
    while(e != NULLENV) {
      Object vars = e.vars;
      Object[] vals = e.vals;
      int i = 0;
      while (U.isPair(vars)) {
	show0(s, U.first(vars), vals[i]);
	vars = U.rest(vars);
	i = i + 1; }
      if (vars != Pair.EMPTY) show0(s, vars, vals[i]);
      e = e.parent;
    }
  }

  private static void show0(java.io.PrintWriter s, Object var, Object val) {
    String sval = showValue(val);
    if (sval.length() > 100) sval = sval.substring(0, 100) + "...";
    s.println("  " + U.stringify(var) + " = " + sval);
    }

  private static String showValue(Object x) {
    if (BacktraceException.checkBacktrace()) {
      Object btvsProc = Scheme.currentEvaluator().getInteractionEnvironment().
	getValue(Symbol.intern("backtraceValueString"));
      return (String) U.toProc(btvsProc).apply(jscheme.JScheme.list(x));
    } else return (x == null) ? "null" : x.toString();
  }
}
