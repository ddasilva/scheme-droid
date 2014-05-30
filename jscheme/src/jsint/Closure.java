package jsint;

/** A closure is a user-defined procedure.  It is "closed" over the
 * environment in which it was created.  To apply the procedure, bind
 * the parameters to the passed in variables, and evaluate the body.
 * @author Peter Norvig, Copyright 2000, peter@norvig.com, <a href="license.txt">license</a>
 * subsequently modified by Jscheme project members
 * licensed under zlib licence (see license.txt)
 */

public class Closure extends Procedure implements Cloneable {

  Object body;
  LexicalEnvironment lexenv;
  Object parms;
    
  /** Make a closure from a body and the analyze time environment it
      was defined in. To create a closure at execute time, use .copy()
      **/
  public Closure (Object parms, Object body, LexicalEnvironment lexenv) { 
    this.parms = parms; this.body = body; this.lexenv = lexenv; 
    this.computeArgs(parms, 0);
  }

  /** Compute minArgs and maxArgs from the parameter list **/
  private void computeArgs(Object parms, int min) {
    if (U.isPair(parms)) computeArgs(((Pair) parms).rest, min + 1);
    else if (parms == Pair.EMPTY) {
      this.minArgs = min;
      this.maxArgs = min; 
    } else {
      this.minArgs = min;
      this.maxArgs = Integer.MAX_VALUE;
    }}

  /** Apply a closure to a vector of arguments. Do this by creating a
   * new LexicalEnvironment containing the arguments, and then
   * evaluating the body of the closure in that new frame.
   **/
   public Object apply(Object[] args) {
     return Scheme.currentEvaluator().execute
       (body, new LexicalEnvironment(parms, args, lexenv));
   }
  
  /** Make a copy, but with a runtime environment. **/
  public Closure copy(LexicalEnvironment lexenv) {
    Closure c = null;
    try { c = (Closure)this.clone(); }
    catch (CloneNotSupportedException e)
      { E.error("internal error: no clone"); }
    c.lexenv = lexenv;
    return c;
  }

  public String toString() {
    return "(lambda " + this.name + " " + U.stringify(parms) + "...)";
  }

  public Object setName(Object name) {
    super.setName(name);
    identifyInternalClosures(name.toString(), 0, body);
    return name;
  }
  
  /** To aid debugging, label internal closures with <name>~<count>. **/
  private static int identifyInternalClosures
    (String name, int count, Object body) {
    if (body instanceof Closure) {
      Closure c = (Closure) body;
      if (c.name == "??") c.name = name + "~"+ count++;
      return identifyInternalClosures(name, count, c.body);
    } else if (body instanceof Object[]) {
      Object[] exp = (Object[]) body;
      for (int i = 0; i < exp.length; i++)
	count = identifyInternalClosures(name, count, exp[i]);
      return count;
    } else return count;
  }
}
