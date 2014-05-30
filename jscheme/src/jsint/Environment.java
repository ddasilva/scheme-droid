package jsint;

/** Environments store mappings from symbols to locations. 
 * At compile time, we can lookup to see if a symbol names a location,
 * and at run time we can get or set the value in a location.
 * @author Peter Norvig, Copyright 1998, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
**/


public class Environment {

  /** The global environment.  All info is in the Symbols themselves. **/
  public static final Environment GLOBAL 
    = new Environment(Pair.EMPTY, null, null);

  /** The parent Environment: where you look if you can't find a variable. **/
  private Environment parent;
  /** A list of variables in this env.  May be dotted or a lone symbol. **/
  private Object vars;
  /** An array of values in a 1-1 correspondance with the vars. **/
  private Object[] vals;
  
  /** Create an Environment with local variables and values, and a parent
   * Environment. **/
  public Environment(Object vars, Object[] vals, Environment parent) {
    this.vars = vars; this.vals = vals; this.parent = parent;
  }
  
  public Object lookup(Symbol var) { return lookup(var, 0, 0, vars); }
 
  /** Lookup the symbol in the environment.  Return either a
   * LocalVariable a Symbol representing the global variable.
  **/
  public Object lookup(Symbol var, int up, int in, Object vars) {
    if (this == GLOBAL) return var; 
    else if (vars == var) return new LocalVariable(up, in, var);
    else if (!U.isPair(vars)) return parent.lookup(var, up+1, 0, parent.vars);
    else if (U.first(vars) == var) return new LocalVariable(up, in, var);
    else return lookup(var, up, in+1, U.rest(vars)); 
  }

  /** Get the value of the LocalVariable in this Environment. **/
  public Object get(LocalVariable var) {
    return this.up(var.up).vals[var.in];
  }
  
  /** Set the value of the LocalVariable in this Environment to a new value. **/
  public Object set(LocalVariable var, Object newVal) {
    return this.up(var.up).vals[var.in] = newVal;
  }  
  
  /** Go up a specified number of levels in the parent chain. **/
  Environment up(int levels) {
    return (levels == 0) ? this : this.parent.up(levels - 1);
  }
  
  public String toString() {
    return "{|" + toString0() + "|}";
  }
  private String toString0() {
    if (this == GLOBAL) return "GLOBAL";
    else return U.stringify(vars) + " " + U.stringify(vals) + 
	   ((parent == null) ? "" : "|" + parent.toString0()); 
  }

  public void show(java.io.PrintWriter s) {
    Environment e = this;
    while(e != GLOBAL) {
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

  private void show0(java.io.PrintWriter s, Object var, Object val) {
    String sval = U.stringify(val);
    if (sval.length() > 100) sval = sval.substring(0, 100) + "...";
    s.println("  " + U.stringify(var) + " = " + sval);
    }
}
